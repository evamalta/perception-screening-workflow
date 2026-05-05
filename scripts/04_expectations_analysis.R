# ============================================================
# 04_expectations_analysis.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   Analyse stakeholder expectations regarding collaborative
#   management of biological invasions.
#
# Workflow:
#   1. Prepare statements dataset
#   2. Calculate intercoder reliability (Cohen's kappa)
#   3. Analyse final category frequencies
#   4. Summarise results by stakeholder sector
#
# Input datasets:
#   expectations_agreement.xlsx
#   expectations.xlsx
#
# ============================================================

# ---------------------------
# 0. Load project setup
# ---------------------------
source("scripts/00_setup.R")

# ---------------------------
# 0.1 Define expectation category levels
# ---------------------------
expectation_category_levels <- c(
  "Access to decision-support tools and technologies",
  "Strengthening collaboration and governance networks",
  "Enabling practical action and implementation",
  "Co-developing and sharing knowledge",
  "Engaging society and raising awareness"
)

# ---------------------------
# 1. Import coder agreement dataset
# ---------------------------
expectations_agreement <- readxl::read_excel(
  file.path(path_data, "expectations_agreement.xlsx")
) %>%
  janitor::clean_names()

check_required_columns(
  expectations_agreement,
  c(
    "participant_id",
    "stakeholder_sector",
    "expectation_nr",
    "expectation",
    "coder1",
    "coder2"
  ),
  "expectations_agreement"
)

# ---------------------------
# 2. Clean coder agreement dataset
# ---------------------------
expectations_agreement <- expectations_agreement %>%
  dplyr::mutate(
    participant_id = as.character(participant_id),
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = stakeholder_sector_levels
    ),
    coder1 = factor(coder1, levels = expectation_category_levels),
    coder2 = factor(coder2, levels = expectation_category_levels)
  ) %>%
  dplyr::filter(
    !is.na(coder1),
    !is.na(coder2)
  )

# ---------------------------
# 3. Intercoder reliability
# ---------------------------
ratings <- expectations_agreement %>%
  dplyr::select(coder1, coder2)

kappa_result <- irr::kappa2(ratings)

kappa_summary <- tibble::tibble(
  statistic = "Cohen's kappa",
  value = kappa_result$value
)

# ---------------------------
# 4. Import final categorised dataset
# ---------------------------
expectations_final <- readxl::read_excel(
  file.path(path_data, "expectations.xlsx")
) %>%
  janitor::clean_names()

check_required_columns(
  expectations_final,
  c(
    "participant_id",
    "stakeholder_sector",
    "expectation_nr",
    "expectation_category"
  ),
  "expectations_final"
)

# ---------------------------
# 5. Clean final categorised dataset
# ---------------------------
expectations_final <- expectations_final %>%
  dplyr::mutate(
    participant_id = as.character(participant_id),
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = stakeholder_sector_levels
    ),
    expectation_category = factor(
      expectation_category,
      levels = expectation_category_levels
    )
  )

# ---------------------------
# 6. Category frequencies
# ---------------------------

# Overall frequencies
category_frequency_total <- expectations_final %>%
  dplyr::count(expectation_category, name = "n_statements") %>%
  dplyr::mutate(
    percent_total = 100 * n_statements / sum(n_statements)
  ) %>%
  dplyr::arrange(expectation_category)

# Frequencies by stakeholder sector
category_frequency_sector <- expectations_final %>%
  dplyr::count(
    stakeholder_sector,
    expectation_category,
    name = "n_statements"
  ) %>%
  dplyr::group_by(stakeholder_sector) %>%
  dplyr::mutate(
    percent_within_sector =
      100 * n_statements / sum(n_statements)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(stakeholder_sector, expectation_category)

# Wide table for reporting
category_frequency_wide <- category_frequency_sector %>%
  tidyr::pivot_wider(
    names_from = stakeholder_sector,
    values_from = n_statements,
    values_fill = 0
  )

# ---------------------------
# 7. Summary statistics
# ---------------------------
expectations_summary <- tibble::tibble(
  total_statements =
    nrow(expectations_final),
  unique_participants =
    dplyr::n_distinct(expectations_final$participant_id),
  number_of_categories =
    dplyr::n_distinct(expectations_final$expectation_category)
)

# ---------------------------
# 8. Visualisation
# ---------------------------

# Overall percentages
plot_data_total <- category_frequency_total %>%
  dplyr::transmute(
    expectation_category,
    stakeholder_sector = "All participants",
    percent = percent_total
  )

# Sector percentages
plot_data_sector <- category_frequency_sector %>%
  dplyr::transmute(
    expectation_category,
    stakeholder_sector = as.character(stakeholder_sector),
    percent = percent_within_sector
  )

# Desired category order: first category at top, fifth at bottom
category_order <- expectation_category_levels

# Desired stakeholder order within each category
stakeholder_order <- c(
  "All participants",
  "Civil Society",
  "Private Sector",
  "Public Sector",
  "Academia"
)

plot_data <- dplyr::bind_rows(plot_data_total, plot_data_sector) %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = stakeholder_order
    ),
    expectation_category = factor(
      expectation_category,
      levels = rev(category_order)
    )
  )

plot_palette <- c(
  "All participants" = "grey50",
  stakeholder_palette
)

expectations_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = expectation_category,
    y = percent,
    fill = stakeholder_sector
  )
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.8),
    width = 0.7
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(
    values = plot_palette
  ) +
  ggplot2::labs(
    title = "Stakeholder expectations for collaborative management",
    x = NULL,
    y = "Relative frequency (%)",
    fill = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 9)
  )

ggplot2::ggsave(
  file.path(path_figures, "expectations_distribution.svg"),
  expectations_plot,
  width = 12,
  height = 7
)

# ---------------------------
# 9. Export results
# ---------------------------
writexl::write_xlsx(
  list(
    kappa_summary = kappa_summary,
    category_frequency_total = category_frequency_total,
    category_frequency_sector = category_frequency_sector,
    category_frequency_wide = category_frequency_wide,
    expectations_summary = expectations_summary
  ),
  file.path(path_tables, "expectations_outputs.xlsx")
)

