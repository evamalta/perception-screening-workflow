# ============================================================
# 00_setup.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   - Load required packages
#   - Define project paths
#   - Define canonical category levels and palettes
#   - Provide helper functions for validation and logging
# ============================================================

# ---------------------------
# 1. Load packages
# ---------------------------
required_packages <- c(
  "tidyverse",
  "readxl",
  "writexl",
  "openxlsx",
  "janitor",
  "vegan",
  "igraph",
  "ggraph",
  "ggrepel",
  "rstatix",
  "irr",
  "sf",
  "raster",
  "corrplot"
)

invisible(
  lapply(required_packages, library, character.only = TRUE)
)

# ---------------------------
# 2. Reproducibility
# ---------------------------
set.seed(122)

# ---------------------------
# 3. Project paths
# ---------------------------
path_data    <- "data"
path_outputs <- "outputs"

path_figures <- file.path(path_outputs, "figures")
path_tables  <- file.path(path_outputs, "tables")
path_rasters <- file.path(path_outputs, "rasters")
path_logs    <- file.path(path_outputs, "logs")

dir.create(path_outputs, showWarnings = FALSE, recursive = TRUE)
dir.create(path_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(path_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(path_rasters, showWarnings = FALSE, recursive = TRUE)
dir.create(path_logs,    showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# 4. Canonical category levels
# ---------------------------
stakeholder_sector_levels <- c(
  "Academia",
  "Public Sector",
  "Private Sector",
  "Civil Society"
)

map_dimension_levels <- c(
  "Occurrence",
  "Impacts",
  "Management",
  "Detection"
)

taxonomic_group_levels <- c(
  "Plants",
  "Other Invertebrates",
  "Insects",
  "Herpetofauna",
  "Birds",
  "Mammals"
)

stakeholder_palette <- c(
  "Academia"      = "#98CAEB",
  "Public Sector" = "#C06B77",
  "Private Sector"= "#60A899",
  "Civil Society" = "#E2B469",
  "Tie"           = "#BEBEBE"
)

# ---------------------------
# 5. Helper functions
# ---------------------------
check_required_columns <- function(data, required_cols, object_name = "data") {
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required columns in ", object_name, ": ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

check_valid_levels <- function(x, valid_levels, var_name = "variable") {
  invalid <- setdiff(unique(stats::na.omit(x)), valid_levels)
  
  if (length(invalid) > 0) {
    stop(
      paste0(
        "Unexpected values in ", var_name, ": ",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

save_session_info <- function(file = file.path(path_logs, "session_info.txt")) {
  writeLines(capture.output(sessionInfo()), con = file)
}

