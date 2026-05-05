# ============================================================
# 02_species_of_concern.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   - Analyse species of concern mentioned by participants
#   - Summarise overall and stakeholder-sector-specific frequencies
#   - Summarise alignment with EU and Portuguese policy lists
#   - Summarise mentions by species group
#   - Export descriptive tables and publication-ready figure
#
# Input:
#   species.xlsx
#
# Expected columns:
#   participant_id
#   stakeholder_sector
#   species
#   species_group
#   in_eu_list
#   in_pt_list
#
# Notes:
#   - Species names were manually standardised prior to analysis
#   - Policy alignment (EU/PT lists) was manually coded prior to analysis
#   - Duplicate mentions within participant × species × stakeholder sector
#     are counted only once
#   - The figure shows, for each species, the proportion of total mentions
#     contributed by each stakeholder sector
# ============================================================

# ---------------------------
# 0. Load project setup
# ---------------------------
source("scripts/00_setup.R")

# ---------------------------
# 1. Import data
# ---------------------------
species_raw <- readxl::read_excel(file.path(path_data, "species.xlsx")) %>%
  janitor::clean_names()

check_required_columns(
  data = species_raw,
  required_cols = c(
    "participant_id",
    "stakeholder_sector",
    "species",
    "species_group",
    "in_eu_list",
    "in_pt_list"
  ),
  object_name = "species_raw"
)

# ---------------------------
# 2. Clean and validate data
# ---------------------------
species_clean <- species_raw %>%
  dplyr::mutate(
    participant_id = as.character(participant_id),
    stakeholder_sector = as.character(stakeholder_sector),
    species = stringr::str_trim(species),
    species_group = stringr::str_trim(species_group),
    in_eu_list = stringr::str_to_lower(stringr::str_trim(in_eu_list)),
    in_pt_list = stringr::str_to_lower(stringr::str_trim(in_pt_list))
  ) %>%
  dplyr::filter(
    !is.na(participant_id),
    !is.na(stakeholder_sector),
    !is.na(species),
    species != "",
    !is.na(species_group),
    species_group != "",
    !is.na(in_eu_list),
    !is.na(in_pt_list)
  ) %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = stakeholder_sector_levels
    )
  )

check_valid_levels(
  x = species_clean$stakeholder_sector,
  valid_levels = stakeholder_sector_levels,
  var_name = "stakeholder_sector"
)

check_valid_levels(
  x = species_clean$in_eu_list,
  valid_levels = c("yes", "no"),
  var_name = "in_eu_list"
)

check_valid_levels(
  x = species_clean$in_pt_list,
  valid_levels = c("yes", "no"),
  var_name = "in_pt_list"
)

# ---------------------------
# 3. Remove duplicate mentions
# ---------------------------
# If the same participant mentions the same species more than once
# within the same stakeholder sector, it is counted once.
species_unique <- species_clean %>%
  dplyr::distinct(
    participant_id,
    stakeholder_sector,
    species,
    species_group,
    in_eu_list,
    in_pt_list
  )

# ---------------------------
# 4. Participant summary
# ---------------------------
participants_by_sector <- species_unique %>%
  dplyr::group_by(stakeholder_sector) %>%
  dplyr::summarise(
    n_participants = dplyr::n_distinct(participant_id),
    .groups = "drop"
  )

total_participants <- species_unique %>%
  dplyr::summarise(
    total_participants = dplyr::n_distinct(participant_id)
  ) %>%
  dplyr::pull(total_participants)

# ---------------------------
# 5. Species mention frequencies by stakeholder sector
# ---------------------------
species_by_sector <- species_unique %>%
  dplyr::group_by(species, species_group, stakeholder_sector) %>%
  dplyr::summarise(
    n_mentions = dplyr::n_distinct(participant_id),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    percent_all_participants = (n_mentions / total_participants) * 100
  )

# ---------------------------
# 6. Overall species frequencies
# ---------------------------
species_totals <- species_unique %>%
  dplyr::group_by(species, species_group, in_eu_list, in_pt_list) %>%
  dplyr::summarise(
    total_mentions = dplyr::n_distinct(participant_id),
    percent_all_participants = (total_mentions / total_participants) * 100,
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(total_mentions), species)

# ---------------------------
# 7. Detailed species table 
# ---------------------------
species_mentions_wide <- species_by_sector %>%
  dplyr::select(species, stakeholder_sector, n_mentions) %>%
  tidyr::pivot_wider(
    names_from = stakeholder_sector,
    values_from = n_mentions,
    values_fill = 0
  )

species_stats <- species_totals %>%
  dplyr::left_join(species_mentions_wide, by = "species") %>%
  dplyr::arrange(dplyr::desc(total_mentions), species)

# ---------------------------
# 8. Policy alignment summaries
# ---------------------------
policy_summary_species <- species_totals %>%
  dplyr::group_by(in_eu_list, in_pt_list) %>%
  dplyr::summarise(
    n_species = dplyr::n(),
    total_mentions = sum(total_mentions),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(total_mentions))

policy_summary_sector <- species_unique %>%
  dplyr::group_by(stakeholder_sector, in_eu_list, in_pt_list) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(stakeholder_sector, dplyr::desc(n_records))

# ---------------------------
# 9. Species-group descriptive statistics
# ---------------------------
species_group_totals <- species_unique %>%
  dplyr::group_by(species_group) %>%
  dplyr::summarise(
    total_mentions = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    participants_mentioning_group = dplyr::n_distinct(participant_id),
    percent_all_participants = (participants_mentioning_group / total_participants) * 100,
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(total_mentions), species_group)

species_group_by_sector <- species_unique %>%
  dplyr::group_by(species_group, stakeholder_sector) %>%
  dplyr::summarise(
    n_mentions = dplyr::n(),
    participants_mentioning_group = dplyr::n_distinct(participant_id),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = stakeholder_sector,
    values_from = c(n_mentions, participants_mentioning_group),
    values_fill = 0
  )

species_group_policy_summary <- species_unique %>%
  dplyr::group_by(species_group, in_eu_list, in_pt_list) %>%
  dplyr::summarise(
    n_mentions = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    .groups = "drop"
  ) %>%
  dplyr::arrange(species_group, dplyr::desc(n_mentions))

# ---------------------------
# 10. Compact sector-level summary
# ---------------------------
species_sector_summary <- species_unique %>%
  dplyr::group_by(stakeholder_sector) %>%
  dplyr::summarise(
    n_participants = dplyr::n_distinct(participant_id),
    total_mentions = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    unique_species_groups = dplyr::n_distinct(species_group),
    mean_mentions_per_species = total_mentions / unique_species,
    .groups = "drop"
  )

species_overall_summary <- tibble::tibble(
  total_participants = total_participants,
  total_mentions = nrow(species_unique),
  unique_species = dplyr::n_distinct(species_unique$species),
  unique_species_groups = dplyr::n_distinct(species_unique$species_group),
  species_in_eu_list = sum(species_totals$in_eu_list == "yes"),
  species_in_pt_list = sum(species_totals$in_pt_list == "yes")
)

# ---------------------------
# 11. Prepare plotting data
# ---------------------------

# Contribution of each stakeholder sector to the total % of participants
species_plot_data <- species_by_sector %>%
  dplyr::mutate(
    percent_total_participants = (n_mentions / total_participants) * 100
  )

# Species ordering based on total salience
species_order <- species_totals %>%
  dplyr::arrange(dplyr::desc(percent_all_participants), species) %>%
  dplyr::pull(species)

species_plot_data <- species_plot_data %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = rev(stakeholder_sector_levels)
    ),
    species = factor(species, levels = rev(species_order))
  )

# ---------------------------
# 12. Plot stacked horizontal bar chart
# ---------------------------
species_plot <- ggplot2::ggplot(
  species_plot_data,
  ggplot2::aes(
    y = species,
    x = percent_total_participants,
    fill = stakeholder_sector
  )
) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::scale_fill_manual(
    name = "Stakeholder sector",
    values = stakeholder_palette[rev(stakeholder_sector_levels)],
    breaks = rev(stakeholder_sector_levels)
  ) +
  ggplot2::labs(
    title = "Species of concern mentioned by participants",
    x = "% of all participants mentioning the species",
    y = "Species"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.title.y = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 9),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(path_figures, "species_of_concern_stacked.svg"),
  plot = species_plot,
  width = 180,
  height = 240,
  units = "mm",
  dpi = 300
)

# ---------------------------
# 13. Export output tables
# ---------------------------
writexl::write_xlsx(
  list(
    species_stats = species_stats,
    species_by_sector = species_by_sector,
    species_totals = species_totals,
    policy_summary_species = policy_summary_species,
    policy_summary_sector = policy_summary_sector,
    species_group_totals = species_group_totals,
    species_group_by_sector = species_group_by_sector,
    species_group_policy_summary = species_group_policy_summary,
    species_sector_summary = species_sector_summary,
    species_overall_summary = species_overall_summary
  ),
  file.path(path_tables, "species_outputs.xlsx")
)
