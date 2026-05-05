# ============================================================
# 05_local_spatial_knowledge.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   Analyse participatory mapping data using kernel density
#   estimation (KDE), hotspot overlap, and spatial correlations.
#
# Purpose:
#   Analyse participatory mapping data using kernel density
#   estimation (KDE), hotspot overlap, and spatial correlations.
#
# Input:
#   spatial_data.shp
#   study_area.shp
#
# Expected attributes in spatial_data:
#   id
#   stake_sec
#   map_dim
#
# Notes:
#   - Sticker polygons were digitised and attributed in QGIS
#   - KDE is computed from polygon centroids
#   - Analysis is performed in EPSG:3763 (ETRS89 / Portugal TM06)
#   - KDE is computed on a buffered extent to reduce edge effects
#   - Final exported rasters are masked to the buffered extent
#   - Spatial correlations are computed using cells where at least
#     one stakeholder sector has non-zero density
# ============================================================

# ---------------------------
# 0. Load project setup
# ---------------------------
source("scripts/00_setup.R")
library(MASS)

# ---------------------------
# 1. Define input files and parameters
# ---------------------------
input_spatial <- file.path(path_data, "spatial_data.shp")
input_study   <- file.path(path_data, "study_area.shp")

crs_target  <- 3763   # ETRS89 / Portugal TM06
pixel_size  <- 1300   # 1.3 km, matching sticker diameter
buffer_dist <- 1300   # one-cell buffer to reduce edge effects
threshold_q <- 0.80   # top 20% of positive normalized values = hotspots

set.seed(42)

# Output folders for this analysis
path_spatial_rasters <- file.path(path_rasters, "local_spatial_knowledge")
path_spatial_tables  <- file.path(path_tables, "local_spatial_knowledge")
path_spatial_figures <- file.path(path_figures, "local_spatial_knowledge")
path_spatial_vectors <- file.path(path_outputs, "vectors")

dir.create(path_spatial_rasters, showWarnings = FALSE, recursive = TRUE)
dir.create(path_spatial_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(path_spatial_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(path_spatial_vectors, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# 2. Import data
# ---------------------------
spatial_raw <- sf::st_read(input_spatial, quiet = TRUE) %>%
  janitor::clean_names()

study_area_raw <- sf::st_read(input_study, quiet = TRUE) %>%
  janitor::clean_names()

check_required_columns(
  data = spatial_raw,
  required_cols = c("id", "stake_sec", "map_dim"),
  object_name = "spatial_raw"
)

# ---------------------------
# 3. Clean and validate data
# ---------------------------
spatial_clean <- spatial_raw %>%
  dplyr::rename(
    stakeholder_sector = stake_sec,
    map_dimension = map_dim
  ) %>%
  sf::st_transform(crs_target) %>%
  dplyr::mutate(
    id = as.character(id),
    stakeholder_sector = as.character(stakeholder_sector),
    map_dimension = as.character(map_dimension)
  )

study_area <- study_area_raw %>%
  sf::st_transform(crs_target)

check_valid_levels(
  x = spatial_clean$stakeholder_sector,
  valid_levels = stakeholder_sector_levels,
  var_name = "stakeholder_sector"
)

check_valid_levels(
  x = spatial_clean$map_dimension,
  valid_levels = map_dimension_levels,
  var_name = "map_dimension"
)

# ---------------------------
# 4. Extract centroid points
# ---------------------------
centroids <- spatial_clean %>%
  sf::st_centroid() %>%
  dplyr::select(id, stakeholder_sector, map_dimension)

sf::st_write(
  centroids,
  dsn = file.path(path_spatial_vectors, "spatial_centroids.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)

# ---------------------------
# 5. Prepare buffered extent and raster grid
# ---------------------------
study_area_buffer <- sf::st_buffer(study_area, dist = buffer_dist)
study_bbox_buffer <- sf::st_bbox(study_area_buffer)

x_seq <- seq(study_bbox_buffer["xmin"], study_bbox_buffer["xmax"], by = pixel_size)
y_seq <- seq(study_bbox_buffer["ymin"], study_bbox_buffer["ymax"], by = pixel_size)

nx <- length(x_seq)
ny <- length(y_seq)

lims <- c(
  study_bbox_buffer["xmin"],
  study_bbox_buffer["xmax"],
  study_bbox_buffer["ymin"],
  study_bbox_buffer["ymax"]
)

study_area_buffer_sp <- as(study_area_buffer, "Spatial")

# ---------------------------
# 6. Global bandwidths and normality diagnostics
# ---------------------------
coords_all <- sf::st_coordinates(centroids)
x_all <- coords_all[, 1]
y_all <- coords_all[, 2]

# Helper for Shapiro-Wilk on large vectors
shapiro_safe <- function(v, max_n = 5000) {
  v_use <- if (length(v) > max_n) sample(v, max_n) else v
  stats::shapiro.test(v_use)
}

sw_x <- shapiro_safe(x_all)
sw_y <- shapiro_safe(y_all)

normality_summary <- tibble::tibble(
  axis = c("x", "y"),
  n_points = c(length(x_all), length(y_all)),
  shapiro_w = c(unname(sw_x$statistic), unname(sw_y$statistic)),
  p_value = c(sw_x$p.value, sw_y$p.value)
)

# Silverman's normal reference rule using MASS implementation
h_x_global <- MASS::bandwidth.nrd(x_all)
h_y_global <- MASS::bandwidth.nrd(y_all)

bandwidth_summary <- tibble::tibble(
  parameter = c("h_x_global", "h_y_global"),
  value = c(h_x_global, h_y_global)
)

# ---------------------------
# 7. KDE per stakeholder sector × map dimension
# ---------------------------
unique_sectors <- unique(spatial_clean$stakeholder_sector)
unique_maps <- unique(spatial_clean$map_dimension)

kde_summary_list <- list()
kde_file_index_list <- list()

for (sector in unique_sectors) {
  for (map_dim in unique_maps) {
    
    message("Processing KDE for: ", sector, " × ", map_dim)
    
    pts <- centroids %>%
      dplyr::filter(
        stakeholder_sector == sector,
        map_dimension == map_dim
      )
    
    if (nrow(pts) < 2) {
      warning("Skipping ", sector, " × ", map_dim, ": fewer than 2 points.")
      next
    }
    
    coords <- sf::st_coordinates(pts)
    
    # 2D KDE on predefined buffered grid using global bandwidths
    kde_res <- MASS::kde2d(
      x = coords[, 1],
      y = coords[, 2],
      h = c(h_x_global, h_y_global),
      n = c(nx, ny),
      lims = lims
    )
    
    kde_df <- expand.grid(
      x = kde_res$x,
      y = kde_res$y
    )
    kde_df$z <- as.vector(kde_res$z)
    
    r_raw <- raster::rasterFromXYZ(kde_df)
    raster::crs(r_raw) <- sf::st_crs(crs_target)$wkt
    
    # Mask to buffered study area 
    r_buffered <- raster::mask(r_raw, study_area_buffer_sp)
    r_buffered[is.na(r_buffered)] <- 0
    
    # Min-max normalization to 0-1 scale
    r_min <- raster::minValue(r_buffered)
    r_max <- raster::maxValue(r_buffered)
    
    if (is.finite(r_min) && is.finite(r_max) && r_max > r_min) {
      r_norm_buffered <- (r_buffered - r_min) / (r_max - r_min)
    } else {
      r_norm_buffered <- r_buffered
    }
    
    raw_file <- file.path(
      path_spatial_rasters,
      paste0("kde_raw_", sector, "_", map_dim, ".tif")
    )
    
    norm_file <- file.path(
      path_spatial_rasters,
      paste0("kde_normalized_", sector, "_", map_dim, ".tif")
    )
    
    raster::writeRaster(r_buffered, raw_file, overwrite = TRUE)
    raster::writeRaster(r_norm_buffered, norm_file, overwrite = TRUE)
    
    kde_summary_list[[paste(sector, map_dim, sep = "_")]] <- tibble::tibble(
      stakeholder_sector = sector,
      map_dimension = map_dim,
      n_points = nrow(pts),
      bandwidth_x = h_x_global,
      bandwidth_y = h_y_global,
      raw_min = r_min,
      raw_max = r_max,
      raw_mean = raster::cellStats(r_buffered, stat = "mean", na.rm = TRUE)
    )
    
    kde_file_index_list[[paste(sector, map_dim, sep = "_")]] <- tibble::tibble(
      stakeholder_sector = sector,
      map_dimension = map_dim,
      raw_file = raw_file,
      normalized_file = norm_file
    )
  }
}

kde_summary <- dplyr::bind_rows(kde_summary_list)
kde_file_index <- dplyr::bind_rows(kde_file_index_list)

# ---------------------------
# 8. Hotspot overlap and consensus/complementarity maps
# ---------------------------
hotspot_thresholds_list <- list()
hotspot_overlap_summary_list <- list()

for (map_dim in unique_maps) {
  
  message("Building hotspot overlap for: ", map_dim)
  
  map_files <- kde_file_index %>%
    dplyr::filter(map_dimension == map_dim)
  
  if (nrow(map_files) < 2) {
    warning("Skipping hotspot overlap for ", map_dim, ": fewer than 2 rasters.")
    next
  }
  
  hotspot_layers <- list()
  
  for (i in seq_len(nrow(map_files))) {
    sector_i <- map_files$stakeholder_sector[i]
    raster_i <- raster::raster(map_files$normalized_file[i])
    
    vals <- raster::values(raster_i)
    positive_vals <- vals[!is.na(vals) & vals > 0]
    
    if (length(positive_vals) == 0) {
      warning("No positive values for ", sector_i, " × ", map_dim, ".")
      next
    }
    
    thr <- as.numeric(stats::quantile(positive_vals, probs = threshold_q, na.rm = TRUE))
    
    hotspot_r <- raster_i >= thr & raster_i > 0
    hotspot_r[is.na(hotspot_r)] <- 0
    
    hotspot_layers[[sector_i]] <- hotspot_r
    
    hotspot_thresholds_list[[paste(sector_i, map_dim, sep = "_")]] <- tibble::tibble(
      stakeholder_sector = sector_i,
      map_dimension = map_dim,
      hotspot_threshold = thr
    )
  }
  
  if (length(hotspot_layers) == 0) next
  
  # Sum of hotspot layers: values 0-4 indicate overlap across sectors
  overlap_raster <- raster::stack(hotspot_layers) %>%
    raster::calc(fun = sum, na.rm = TRUE)
  
  # Classified overlap raster for interpretation
  classified_raster <- overlap_raster
  overlap_vals <- raster::values(overlap_raster)
  
  class_vals <- dplyr::case_when(
    is.na(overlap_vals) ~ NA_real_,
    overlap_vals == 0 ~ 0,
    overlap_vals %in% c(1, 2) ~ overlap_vals,
    overlap_vals %in% c(3, 4) ~ overlap_vals,
    TRUE ~ overlap_vals
  )
  
  classified_raster[] <- class_vals
  
  overlap_file <- file.path(
    path_spatial_rasters,
    paste0("hotspot_overlap_", map_dim, ".tif")
  )
  
  classified_file <- file.path(
    path_spatial_rasters,
    paste0("hotspot_overlap_classified_", map_dim, ".tif")
  )
  
  raster::writeRaster(
    overlap_raster,
    overlap_file,
    datatype = "INT1U",
    options = c("COMPRESS=DEFLATE"),
    overwrite = TRUE
  )
  
  raster::writeRaster(
    classified_raster,
    classified_file,
    datatype = "INT1U",
    options = c("COMPRESS=DEFLATE"),
    overwrite = TRUE
  )
  
  overlap_freq <- as.data.frame(
    table(raster::values(overlap_raster)),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::rename(
      overlap_value = Var1,
      n_cells = Freq
    ) %>%
    dplyr::mutate(
      overlap_value = as.numeric(overlap_value),
      map_dimension = map_dim,
      interpretation = dplyr::case_when(
        overlap_value == 0 ~ "No hotspot",
        overlap_value %in% c(1, 2) ~ "Complementarity",
        overlap_value %in% c(3, 4) ~ "Consensus",
        TRUE ~ "Other"
      )
    )
  
  hotspot_overlap_summary_list[[map_dim]] <- overlap_freq
}

hotspot_thresholds <- dplyr::bind_rows(hotspot_thresholds_list)
hotspot_overlap_summary <- dplyr::bind_rows(hotspot_overlap_summary_list)

# ---------------------------
# 9. Spatial correlations by map dimension
# ---------------------------
correlation_summary_list <- list()

for (map_dim in unique_maps) {
  
  message("Computing spatial correlations for: ", map_dim)
  
  map_files <- kde_file_index %>%
    dplyr::filter(map_dimension == map_dim)
  
  if (nrow(map_files) < 2) {
    warning("Skipping correlations for ", map_dim, ": fewer than 2 rasters.")
    next
  }
  
  raster_list <- lapply(map_files$normalized_file, raster::raster)
  names(raster_list) <- map_files$stakeholder_sector
  
  raster_stack <- raster::stack(raster_list)
  vals <- raster::getValues(raster_stack)
  
  # Keep cells where at least one stakeholder sector has non-zero density
  keep <- apply(vals, 1, function(z) any(!is.na(z) & z > 0))
  vals_sub <- vals[keep, , drop = FALSE]
  
  if (nrow(vals_sub) < 2) {
    warning("Skipping correlations for ", map_dim, ": insufficient non-zero cells.")
    next
  }
  
  cor_mat <- stats::cor(
    vals_sub,
    method = "spearman",
    use = "pairwise.complete.obs"
  )
  
  cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE) %>%
    dplyr::rename(
      stakeholder_sector_1 = Var1,
      stakeholder_sector_2 = Var2,
      spearman_rho = Freq
    ) %>%
    dplyr::mutate(
      map_dimension = map_dim
    )
  
  correlation_summary_list[[map_dim]] <- cor_df
  
  utils::write.csv(
    cor_mat,
    file = file.path(path_spatial_tables, paste0("spearman_correlation_", map_dim, ".csv")),
    row.names = TRUE
  )
  
  svg(
    filename = file.path(path_spatial_figures, paste0("spearman_correlation_", map_dim, ".svg")),
    width = 6,
    height = 6
  )
  
  corrplot::corrplot(
    cor_mat,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    number.cex = 0.7,
    mar = c(0, 0, 2, 0),
    title = paste("Spearman correlations -", map_dim),
    col = grDevices::colorRampPalette(c("blue", "white", "red"))(200)
  )
  
  grDevices::dev.off()
}

correlation_summary <- dplyr::bind_rows(correlation_summary_list)

# ---------------------------
# 10. Input summaries
# ---------------------------
spatial_input_summary <- centroids %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(stakeholder_sector, map_dimension) %>%
  dplyr::summarise(
    n_centroids = dplyr::n(),
    .groups = "drop"
  )

spatial_input_summary_overall <- centroids %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(
    total_centroids = dplyr::n(),
    unique_stakeholder_sectors = dplyr::n_distinct(stakeholder_sector),
    unique_map_dimensions = dplyr::n_distinct(map_dimension)
  )

# ---------------------------
# 11. Export summary tables
# ---------------------------
writexl::write_xlsx(
  list(
    spatial_input_summary = spatial_input_summary,
    spatial_input_summary_overall = spatial_input_summary_overall,
    normality_summary = normality_summary,
    bandwidth_summary = bandwidth_summary,
    kde_summary = kde_summary,
    kde_file_index = kde_file_index,
    hotspot_thresholds = hotspot_thresholds,
    hotspot_overlap_summary = hotspot_overlap_summary,
    correlation_summary = correlation_summary
  ),
  file.path(path_spatial_tables, "local_spatial_knowledge_outputs.xlsx")
)

