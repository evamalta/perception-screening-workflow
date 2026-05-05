# ============================================================
# 01_framing_analysis.R
# Project: A perception screening framework to inform local governance of biological invasions: insights from a multisectoral community in northern Portugal
# Author: Eva Malta Pinto
#
# Purpose:
#   - Analyse framing words by stakeholder sector
#   - Calculate lexical diversity metrics
#   - Build a descriptive word co-occurrence network
#   - Generate sector-specific structural overlays of the same network
#   - Export detailed descriptive statistics for words and edges
#
# Input:
#   words.xlsx
#
# Expected columns:
#   participant_id
#   stakeholder_sector
#   word
#   word_eng
#
# Notes:
#   - Manual cleaning and harmonisation were performed before analysis
#   - Lowercasing of Portuguese words is performed here in code
#   - The network is built using Portuguese words as analytical units
#   - English translations (word_eng) are used only for node labels
#     in the overall network only
#   - Co-occurrence pairs are built within participants using unique words only
#   - Sector-specific overlays preserve the exact same network structure,
#     node sizes, edge widths, and layout as the overall network
# ============================================================

# ---------------------------
# 0. Load project setup
# ---------------------------
source("scripts/00_setup.R")

# ---------------------------
# 1. Import data
# ---------------------------
words_raw <- readxl::read_excel(file.path(path_data, "words.xlsx")) %>%
  janitor::clean_names()

check_required_columns(
  data = words_raw,
  required_cols = c("participant_id", "stakeholder_sector", "word", "word_eng"),
  object_name = "words_raw"
)

# ---------------------------
# 2. Clean and validate data
# ---------------------------
words_clean <- words_raw %>%
  dplyr::mutate(
    participant_id = as.character(participant_id),
    stakeholder_sector = as.character(stakeholder_sector),
    word = stringr::str_trim(word),
    word = stringr::str_to_lower(word),
    word_eng = stringr::str_trim(word_eng)
  ) %>%
  dplyr::filter(
    !is.na(participant_id),
    !is.na(stakeholder_sector),
    !is.na(word),
    word != ""
  ) %>%
  dplyr::mutate(
    stakeholder_sector = factor(
      stakeholder_sector,
      levels = stakeholder_sector_levels
    )
  )

check_valid_levels(
  x = words_clean$stakeholder_sector,
  valid_levels = stakeholder_sector_levels,
  var_name = "stakeholder_sector"
)

# Check whether each Portuguese word has a unique English translation
word_translation_check <- words_clean %>%
  dplyr::distinct(word, word_eng) %>%
  dplyr::group_by(word) %>%
  dplyr::summarise(
    n_translations = dplyr::n_distinct(word_eng),
    .groups = "drop"
  )

if (any(word_translation_check$n_translations > 1)) {
  warning(
    "Some Portuguese words have more than one English translation in 'word_eng'. ",
    "This may create ambiguous node labels in the co-occurrence network."
  )
}

# Translation lookup table
word_labels <- words_clean %>%
  dplyr::distinct(word, word_eng) %>%
  dplyr::rename(
    name = word,
    label_eng = word_eng
  )

# ---------------------------
# 3. Participant summary
# ---------------------------
participants_by_sector <- words_clean %>%
  dplyr::group_by(stakeholder_sector) %>%
  dplyr::summarise(
    n_participants = dplyr::n_distinct(participant_id),
    .groups = "drop"
  )

total_participants <- words_clean %>%
  dplyr::summarise(
    n_participants = dplyr::n_distinct(participant_id)
  ) %>%
  dplyr::pull(n_participants)

# ---------------------------
# 4. Community matrix
# ---------------------------
community_matrix <- words_clean %>%
  dplyr::count(stakeholder_sector, word, name = "n") %>%
  tidyr::pivot_wider(
    names_from = word,
    values_from = n,
    values_fill = 0
  ) %>%
  tibble::column_to_rownames("stakeholder_sector") %>%
  as.matrix()

community_matrix_total <- rbind(
  community_matrix,
  `Total participants` = colSums(community_matrix)
)

# ---------------------------
# 5. Lexical diversity metrics
# ---------------------------
framing_diversity <- tibble::tibble(
  stakeholder_sector = rownames(community_matrix_total)
) %>%
  dplyr::mutate(
    total_words = rowSums(community_matrix_total),
    richness = vegan::specnumber(community_matrix_total),
    shannon = vegan::diversity(community_matrix_total, index = "shannon"),
    simpson = vegan::diversity(community_matrix_total, index = "simpson"),
    type_token_ratio = richness / total_words
  ) %>%
  dplyr::left_join(participants_by_sector, by = "stakeholder_sector") %>%
  dplyr::mutate(
    n_participants = ifelse(
      stakeholder_sector == "Total participants",
      total_participants,
      n_participants
    )
  )

# ---------------------------
# 6. Co-occurrence pairs by participant
# ---------------------------
participant_pairs <- words_clean %>%
  dplyr::distinct(participant_id, stakeholder_sector, word) %>%
  dplyr::group_by(participant_id, stakeholder_sector) %>%
  dplyr::summarise(
    pairs = list({
      w <- sort(unique(word))
      if (length(w) < 2) {
        list()
      } else {
        asplit(utils::combn(w, 2), 2)
      }
    }),
    .groups = "drop"
  ) %>%
  tidyr::unnest(pairs) %>%
  dplyr::mutate(
    item1 = purrr::map_chr(pairs, 1),
    item2 = purrr::map_chr(pairs, 2)
  ) %>%
  dplyr::select(participant_id, stakeholder_sector, item1, item2)

# ---------------------------
# 7. Edge frequencies and dominant sector
# ---------------------------
edges_by_sector <- participant_pairs %>%
  dplyr::group_by(stakeholder_sector, item1, item2) %>%
  dplyr::summarise(
    n_sector = dplyr::n(),
    .groups = "drop"
  )

edge_totals <- participant_pairs %>%
  dplyr::count(item1, item2, name = "n_total")

edge_dominance <- edges_by_sector %>%
  dplyr::group_by(item1, item2) %>%
  dplyr::slice_max(n_sector, with_ties = TRUE) %>%
  dplyr::mutate(
    dominant_sector = ifelse(
      dplyr::n() > 1,
      "Tie",
      as.character(stakeholder_sector)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(item1, item2, dominant_sector) %>%
  dplyr::left_join(edge_totals, by = c("item1", "item2"))

# ---------------------------
# 8. Node frequencies and dominant sector
# ---------------------------
node_frequency <- words_clean %>%
  dplyr::count(word, name = "freq") %>%
  dplyr::rename(name = word)

node_dominance <- words_clean %>%
  dplyr::group_by(word, stakeholder_sector) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::group_by(word) %>%
  dplyr::slice_max(n, with_ties = TRUE) %>%
  dplyr::mutate(
    dominant_sector = ifelse(
      dplyr::n() > 1,
      "Tie",
      as.character(stakeholder_sector)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(word, dominant_sector) %>%
  dplyr::rename(name = word)

nodes <- node_frequency %>%
  dplyr::left_join(node_dominance, by = "name") %>%
  dplyr::left_join(word_labels, by = "name") %>%
  dplyr::distinct(name, .keep_all = TRUE)

# ---------------------------
# 9. Detailed word-level descriptive statistics
# ---------------------------
word_mentions_by_sector <- words_clean %>%
  dplyr::count(word, stakeholder_sector, name = "n_mentions") %>%
  tidyr::pivot_wider(
    names_from = stakeholder_sector,
    values_from = n_mentions,
    values_fill = 0
  )

word_stats <- node_frequency %>%
  dplyr::rename(total_mentions = freq) %>%
  dplyr::left_join(word_labels, by = "name") %>%
  dplyr::left_join(node_dominance, by = "name") %>%
  dplyr::left_join(word_mentions_by_sector, by = c("name" = "word")) %>%
  dplyr::rename(
    word = name,
    word_eng = label_eng
  ) %>%
  dplyr::arrange(dplyr::desc(total_mentions), word)

# ---------------------------
# 10. Detailed edge-level descriptive statistics
# ---------------------------
edge_mentions_by_sector <- participant_pairs %>%
  dplyr::count(item1, item2, stakeholder_sector, name = "n_mentions") %>%
  tidyr::pivot_wider(
    names_from = stakeholder_sector,
    values_from = n_mentions,
    values_fill = 0
  )

edge_stats_detailed <- edge_totals %>%
  dplyr::rename(total_mentions = n_total) %>%
  dplyr::left_join(edge_dominance, by = c("item1", "item2")) %>%
  dplyr::left_join(edge_mentions_by_sector, by = c("item1", "item2")) %>%
  dplyr::arrange(dplyr::desc(total_mentions), item1, item2)

# ---------------------------
# 11. Compact sector-level summaries
# ---------------------------
node_stats_sector <- words_clean %>%
  dplyr::group_by(stakeholder_sector) %>%
  dplyr::summarise(
    total_word_mentions = dplyr::n(),
    unique_words = dplyr::n_distinct(word),
    mean_mentions_per_word = total_word_mentions / unique_words,
    .groups = "drop"
  )

node_stats_total <- words_clean %>%
  dplyr::summarise(
    stakeholder_sector = "All sectors",
    total_word_mentions = dplyr::n(),
    unique_words = dplyr::n_distinct(word),
    mean_mentions_per_word = total_word_mentions / unique_words
  )

node_stats <- dplyr::bind_rows(node_stats_sector, node_stats_total)

edge_stats_sector <- participant_pairs %>%
  dplyr::group_by(stakeholder_sector) %>%
  dplyr::summarise(
    total_edge_mentions = dplyr::n(),
    unique_edges = dplyr::n_distinct(interaction(item1, item2, drop = TRUE)),
    mean_edge_mentions = total_edge_mentions / unique_edges,
    .groups = "drop"
  )

edge_stats_total <- participant_pairs %>%
  dplyr::summarise(
    stakeholder_sector = "All sectors",
    total_edge_mentions = dplyr::n(),
    unique_edges = dplyr::n_distinct(interaction(item1, item2, drop = TRUE)),
    mean_edge_mentions = total_edge_mentions / unique_edges
  )

edge_stats <- dplyr::bind_rows(edge_stats_sector, edge_stats_total)

# ---------------------------
# 12. Overall network summary
# ---------------------------
network_summary <- tibble::tibble(
  total_nodes = nrow(nodes),
  total_edges = nrow(edge_dominance)
)

# ---------------------------
# 13. Build overall co-occurrence network
# ---------------------------
framing_graph <- igraph::graph_from_data_frame(
  d = edge_dominance,
  directed = FALSE,
  vertices = nodes
)

framing_graph <- igraph::simplify(
  framing_graph,
  remove.multiple = TRUE,
  edge.attr.comb = list(
    n_total = "sum",
    dominant_sector = "first"
  )
)

# Fixed layout used for the overall network and all sector-specific overlays
layout_fr <- ggraph::create_layout(framing_graph, layout = "fr")

min_freq <- min(layout_fr$freq, na.rm = TRUE)
max_freq <- max(layout_fr$freq, na.rm = TRUE)

framing_network_plot <- ggraph::ggraph(layout_fr) +
  ggraph::geom_edge_link(
    ggplot2::aes(width = n_total, colour = dominant_sector),
    alpha = 0.7
  ) +
  ggraph::geom_node_point(
    ggplot2::aes(size = freq, colour = dominant_sector)
  ) +
  ggraph::geom_node_text(
    ggplot2::aes(label = label_eng),
    repel = TRUE
  ) +
  ggraph::scale_edge_width_continuous(
    name = "Co-occurrence strength",
    range = c(0.5, 4)
  ) +
  ggraph::scale_edge_colour_manual(
    values = stakeholder_palette,
    name = "Edge dominant sector"
  ) +
  ggplot2::scale_colour_manual(
    values = stakeholder_palette,
    name = "Node dominant sector"
  ) +
  ggplot2::scale_size_continuous(
    name = "Word frequency",
    range = c(3, 11),
    breaks = c(min_freq, max_freq),
    labels = c(
      paste0("min (", min_freq, ")"),
      paste0("max (", max_freq, ")")
    )
  ) +
  ggraph::theme_graph() +
  ggplot2::labs(
    title = "Word co-occurrence network by dominant stakeholder sector"
  )

ggplot2::ggsave(
  filename = file.path(path_figures, "framing_cooccurrence_network.svg"),
  plot = framing_network_plot,
  width = 10,
  height = 8
)

# Preserve final network objects explicitly for reproducibility
nodes_final <- layout_fr %>%
  dplyr::select(name, label_eng, freq, dominant_sector, x, y)

edges_final <- igraph::as_data_frame(framing_graph, what = "edges") %>%
  dplyr::rename(
    item1 = from,
    item2 = to
  )

# ---------------------------
# 14. Sector-specific network contributions
# ---------------------------
# These summaries quantify the contribution of each stakeholder sector
# to nodes and edges already present in the overall final network.
# The overall network structure and layout are preserved exactly.

node_sector_contributions <- words_clean %>%
  dplyr::count(stakeholder_sector, word, name = "n_sector") %>%
  dplyr::rename(name = word)

edge_sector_contributions <- participant_pairs %>%
  dplyr::count(stakeholder_sector, item1, item2, name = "n_sector")

edge_plot_base <- edges_final %>%
  dplyr::left_join(
    nodes_final %>%
      dplyr::select(name, x, y) %>%
      dplyr::rename(
        x_from = x,
        y_from = y
      ),
    by = c("item1" = "name")
  ) %>%
  dplyr::left_join(
    nodes_final %>%
      dplyr::select(name, x, y) %>%
      dplyr::rename(
        x_to = x,
        y_to = y
      ),
    by = c("item2" = "name")
  )

# Function to generate one sector-specific structural overlay
plot_sector_network <- function(sector_name) {
  
  sector_colour <- stakeholder_palette[[sector_name]]
  
  nodes_sector_plot <- nodes_final %>%
    dplyr::left_join(
      node_sector_contributions %>%
        dplyr::filter(stakeholder_sector == sector_name) %>%
        dplyr::select(name, n_sector),
      by = "name"
    ) %>%
    dplyr::mutate(
      n_sector = dplyr::coalesce(n_sector, 0L),
      contributed = n_sector > 0
    )
  
  edges_sector_plot <- edge_plot_base %>%
    dplyr::left_join(
      edge_sector_contributions %>%
        dplyr::filter(stakeholder_sector == sector_name) %>%
        dplyr::select(item1, item2, n_sector),
      by = c("item1", "item2")
    ) %>%
    dplyr::mutate(
      n_sector = dplyr::coalesce(n_sector, 0L),
      contributed = n_sector > 0
    )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges_sector_plot,
      ggplot2::aes(
        x = x_from, y = y_from,
        xend = x_to, yend = y_to,
        linewidth = n_total
      ),
      colour = "grey85",
      alpha = 0.45
    ) +
    ggplot2::geom_segment(
      data = dplyr::filter(edges_sector_plot, contributed),
      ggplot2::aes(
        x = x_from, y = y_from,
        xend = x_to, yend = y_to,
        linewidth = n_total
      ),
      colour = sector_colour,
      alpha = 0.95
    ) +
    ggplot2::geom_point(
      data = nodes_sector_plot,
      ggplot2::aes(x = x, y = y, size = freq),
      colour = "grey80",
      alpha = 0.65
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(nodes_sector_plot, contributed),
      ggplot2::aes(x = x, y = y, size = freq),
      colour = sector_colour,
      alpha = 0.98
    ) +
    ggplot2::scale_size_continuous(
      range = c(3, 11),
      guide = "none"
    ) +
    ggplot2::scale_linewidth_continuous(
      range = c(0.5, 4),
      guide = "none"
    ) +
    ggraph::theme_graph() +
    ggplot2::labs(
      title = sector_name
    )
  
  ggplot2::ggsave(
    filename = file.path(
      path_figures,
      paste0(
        "framing_cooccurrence_network_",
        stringr::str_replace_all(
          stringr::str_to_lower(sector_name),
          " ",
          "_"
        ),
        ".svg"
      )
    ),
    plot = p,
    width = 10,
    height = 8
  )
  
  return(p)
}

# Generate sector-specific plots
sector_network_plots <- stats::setNames(
  lapply(stakeholder_sector_levels, plot_sector_network),
  stakeholder_sector_levels
)

# ---------------------------
# 15. Export output tables
# ---------------------------
writexl::write_xlsx(
  list(
    framing_diversity = framing_diversity,
    word_stats = word_stats,
    edge_stats_detailed = edge_stats_detailed,
    node_attributes = nodes,
    edge_attributes = edge_dominance,
    node_stats = node_stats,
    edge_stats = edge_stats,
    network_summary = network_summary,
    node_sector_contributions = node_sector_contributions,
    edge_sector_contributions = edge_sector_contributions
  ),
  file.path(path_tables, "framing_outputs.xlsx")
)

