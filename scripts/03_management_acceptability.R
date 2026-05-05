# ============================================================
# 03_management_acceptability.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   Analyse stakeholder comfort with eradication actions across
#   different taxonomic groups using nonparametric statistics.
#
# Input:
#   comfort.xlsx
#
# Expected columns:
#   participant_id
#   stakeholder_sector
#   plants
#   mammals
#   birds
#   herpetofauna
#   insects
#   other_invertebrates
#
# Analysis steps:
#   1. Data cleaning and reshaping
#   2. Descriptive statistics (median, IQR, frequencies)
#   3. Kruskal–Wallis tests across stakeholder sectors
#   4. Dunn post-hoc comparisons
#   5. Friedman test across taxa (within participants)
#   6. Paired Wilcoxon post-hoc tests
#   7. Publication-quality visualisations
# ============================================================

# ---------------------------
# 0. Load project setup
# ---------------------------
source("scripts/00_setup.R")

# ---------------------------
# 1. Import data
# ---------------------------
comfort_raw <- readxl::read_excel(file.path(path_data, "comfort.xlsx")) %>%
  janitor::clean_names()

check_required_columns(
  data = comfort_raw,
  required_cols = c(
    "participant_id",
    "stakeholder_sector",
    "plants",
    "mammals",
    "birds",
    "herpetofauna",
    "insects",
    "other_invertebrates"
  ),
  object_name = "comfort_raw"
)

# ---------------------------
# 2. Clean and validate data
# ---------------------------
comfort_clean <- comfort_raw %>%
  dplyr::mutate(
    participant_id = as.character(participant_id),
    stakeholder_sector = as.character(stakeholder_sector)
  ) %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = stakeholder_sector_levels
    )
  )

check_valid_levels(
  comfort_clean$stakeholder_sector,
  stakeholder_sector_levels,
  "stakeholder_sector"
)

# ---------------------------
# 3. Reshape to long format
# ---------------------------
taxon_levels <- c(
  "plants",
  "other_invertebrates",
  "insects",
  "herpetofauna",
  "birds",
  "mammals"
)

comfort_long <- comfort_clean %>%
  tidyr::pivot_longer(
    cols = c(
      plants,
      mammals,
      birds,
      herpetofauna,
      insects,
      other_invertebrates
    ),
    names_to = "taxon",
    values_to = "comfort"
  ) %>%
  dplyr::mutate(
    taxon = factor(taxon, levels = taxon_levels),
    comfort = as.numeric(comfort),
    participant_id = factor(participant_id)
  ) %>%
  dplyr::filter(!is.na(comfort))

# Optional integrity check: expected Likert values should be 1–5
invalid_comfort_values <- setdiff(unique(comfort_long$comfort), 1:5)
if (length(invalid_comfort_values) > 0) {
  warning(
    "Unexpected comfort values found: ",
    paste(invalid_comfort_values, collapse = ", ")
  )
}

# ---------------------------
# 4. Descriptive statistics
# ---------------------------

# Relative frequencies of Likert scores by stakeholder sector
comfort_relative_freq <- comfort_long %>%
  dplyr::group_by(stakeholder_sector, taxon, comfort) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop_last"
  ) %>%
  dplyr::mutate(
    relative_frequency = 100 * n / sum(n)
  ) %>%
  dplyr::ungroup()

# Median and IQR by stakeholder sector × taxon
comfort_summary <- comfort_long %>%
  dplyr::group_by(stakeholder_sector, taxon) %>%
  dplyr::summarise(
    n_responses = dplyr::n(),
    median_comfort = stats::median(comfort, na.rm = TRUE),
    iqr_comfort = stats::IQR(comfort, na.rm = TRUE),
    .groups = "drop"
  )

# Overall frequencies (all participants pooled)
comfort_relative_freq_overall <- comfort_long %>%
  dplyr::group_by(taxon, comfort) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop_last"
  ) %>%
  dplyr::mutate(
    relative_frequency = 100 * n / sum(n),
    stakeholder_sector = "All participants"
  ) %>%
  dplyr::ungroup()

# Combined frequency table for plotting/export
comfort_relative_freq_full <- dplyr::bind_rows(
  comfort_relative_freq,
  comfort_relative_freq_overall
) %>%
  dplyr::mutate(
    comfort = factor(comfort, levels = 1:5),
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = c(stakeholder_sector_levels, "All participants")
    )
  )

# Overall median and IQR (all participants pooled)
comfort_summary_overall <- comfort_long %>%
  dplyr::group_by(taxon) %>%
  dplyr::summarise(
    stakeholder_sector = "All participants",
    n_responses = dplyr::n(),
    median_comfort = stats::median(comfort, na.rm = TRUE),
    iqr_comfort = stats::IQR(comfort, na.rm = TRUE),
    .groups = "drop"
  )

# Combined summary table
comfort_summary_full <- dplyr::bind_rows(
  comfort_summary,
  comfort_summary_overall
) %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = c(stakeholder_sector_levels, "All participants")
    )
  )

# ---------------------------
# 5. Kruskal–Wallis tests
# ---------------------------
kw_by_taxon <- comfort_long %>%
  dplyr::group_by(taxon) %>%
  rstatix::kruskal_test(comfort ~ stakeholder_sector) %>%
  dplyr::ungroup()

# Dunn post-hoc comparisons
dunn_by_taxon <- comfort_long %>%
  dplyr::group_by(taxon) %>%
  rstatix::dunn_test(
    comfort ~ stakeholder_sector,
    p.adjust.method = "holm"
  ) %>%
  dplyr::ungroup()

# ---------------------------
# 6. Friedman test across taxa
# ---------------------------
friedman_taxa <- comfort_long %>%
  rstatix::friedman_test(
    comfort ~ taxon | participant_id
  )

# Paired Wilcoxon post-hoc comparisons
wilcox_taxa_pairs <- comfort_long %>%
  rstatix::pairwise_wilcox_test(
    comfort ~ taxon,
    paired = TRUE,
    p.adjust.method = "holm"
  )

# ---------------------------
# 7. Histogram visualisation
# ---------------------------
freq_all <- comfort_relative_freq_full

hist_plot <- ggplot2::ggplot(
  freq_all,
  ggplot2::aes(
    x = comfort,
    y = relative_frequency,
    fill = comfort
  )
) +
  ggplot2::geom_col(
    width = 1,
    colour = "black",
    alpha = 0.85
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(taxon),
    cols = ggplot2::vars(stakeholder_sector)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  ggplot2::scale_fill_brewer(
    palette = "Greys",
    name = "Comfort rating"
  ) +
  ggplot2::labs(
    title = "Relative frequency of comfort scores (1–5)",
    x = "Comfort rating",
    y = "Relative frequency (%)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(size = 8),
    legend.position = "right"
  )

ggplot2::ggsave(
  filename = file.path(path_figures, "comfort_histograms.svg"),
  plot = hist_plot,
  width = 16,
  height = 12,
  dpi = 300
)

# ---------------------------
# 8. Median heatmap
# ---------------------------
median_heatmap <- comfort_summary_full %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = c(stakeholder_sector_levels, "All participants")
    )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = stakeholder_sector,
      y = taxon,
      fill = median_comfort
    )
  ) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(
    ggplot2::aes(label = round(median_comfort, 1)),
    size = 3
  ) +
  ggplot2::scale_fill_gradientn(
    colours = c("#BF4707", "#CEB1C0", "#46346D"),
    values = scales::rescale(c(1, 3, 5)),
    limits = c(1, 5),
    name = "Median comfort"
  ) +
  ggplot2::labs(
    x = "Stakeholder sector",
    y = "Taxonomic group",
    title = "Median comfort in eradication actions"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
  )

ggplot2::ggsave(
  filename = file.path(path_figures, "comfort_heatmap.svg"),
  plot = median_heatmap,
  width = 10,
  height = 6
)

# ---------------------------
# 9. Export results
# ---------------------------
writexl::write_xlsx(
  list(
    comfort_summary = comfort_summary,
    comfort_summary_overall = comfort_summary_overall,
    comfort_summary_full = comfort_summary_full,
    relative_frequencies = comfort_relative_freq,
    relative_frequencies_overall = comfort_relative_freq_overall,
    relative_frequencies_full = comfort_relative_freq_full,
    kruskal_wallis = kw_by_taxon,
    dunn_posthoc = dunn_by_taxon,
    friedman_test = friedman_taxa,
    wilcoxon_pairs = wilcox_taxa_pairs
  ),
  file.path(path_tables, "management_acceptability_outputs.xlsx")
)

