# ============================================================================
# FILE: R/helpers.R
# PURPOSE: Core helper functions for data processing, summarization, and plotting
# DESCRIPTION: Contains reusable functions for filtering data, computing 
#              summaries, and creating visualizations
# ============================================================================

# ============================================================================
# SHARED FUNCTIONS - Used across multiple tabs
# ============================================================================

# Filter checklist data by gender and age range
# Used by: Top Items Proportion, Top Items Table, Age Bin Comparison
filter_checklist <- function(data, genders, age_range) {
  data |>
    filter(
      gender_clean %in% genders,
      age_num >= age_range[1],
      age_num <= age_range[2]
    )
}

# Count participants by item and calculate percentages
# Used by: Top Items Proportion, Top Items Table
count_by_item <- function(data) {
  total_participants <- data |> distinct(participant_id) |> nrow()
  
  data |>
    group_by(clean_item_name) |>
    summarize(count = n_distinct(participant_id), .groups = "drop") |>
    mutate(percent = count / total_participants)
}

# ============================================================================
# TOP ITEMS PROPORTION - Functions for top_items_proportion.R
# ============================================================================

# Plot top items with single color
plot_top_items <- function(df) {
  ggplot(df, aes(x = reorder(clean_item_name, percent), y = percent)) +
    geom_col(fill = "#4A90E2", width = 0.7, alpha = 0.9) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Proportion of Participants",
      title = "Top Collected Items"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      axis.text.y = element_text(size = 12, color = "gray20"),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray98", color = NA)
    )
}

# Plot top items colored by gender (side-by-side bars)
plot_top_items_by_gender <- function(data, n_items) {
  
  gender_colors <- c("girl" = "#C06C84", "boy" = "#0D677C")
  
  # Get total participants per gender
  totals <- data |>
    group_by(gender_clean) |>
    summarize(total = n_distinct(participant_id), .groups = "drop")
  
  # Calculate proportions by gender
  summary_by_gender <- data |>
    group_by(clean_item_name, gender_clean) |>
    summarize(count = n_distinct(participant_id), .groups = "drop") |>
    left_join(totals, by = "gender_clean") |>
    mutate(percent = count / total)
  
  # Get top N items overall (regardless of gender)
  top_items <- data |>
    count_by_item() |>
    slice_max(percent, n = n_items) |>
    pull(clean_item_name)
  
  # Filter to just those top items
  plot_data <- summary_by_gender |>
    filter(clean_item_name %in% top_items)
  
  # Create plot
  ggplot(plot_data, aes(x = reorder(clean_item_name, percent), y = percent, fill = gender_clean)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
    coord_flip() +
    scale_fill_manual(values = gender_colors, name = "Gender") +
    labs(
      x = NULL,
      y = "Proportion of Participants",
      title = "Top Collected Items by Gender"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      axis.text.y = element_text(size = 12, color = "gray20"),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray98", color = NA)
    )
}

# ============================================================================
# TOP ITEMS TABLE - Functions for top_items_table.R
# ============================================================================

# Compute top items by gender with rank, count, and percent
compute_top_items_by_gender <- function(data, n_items) {
  data |>
    group_by(gender_clean) |>
    mutate(total = n_distinct(participant_id)) |>
    group_by(gender_clean, clean_item_name, total) |>
    summarize(count = n_distinct(participant_id), .groups = "drop") |>
    mutate(percent = count / total) |>
    group_by(gender_clean) |>
    slice_max(order_by = count, n = n_items, with_ties = FALSE) |>
    arrange(gender_clean, desc(count)) |>
    group_by(gender_clean) |>
    mutate(rank = row_number()) |>
    ungroup()
}

# Create reactable for single gender
create_top_items_table <- function(data, color) {
  reactable(
    data |> select(rank, clean_item_name, count, percent),
    columns = list(
      rank = colDef(
        name = "Rank",
        width = 60,
        style = list(fontWeight = "bold")
      ),
      clean_item_name = colDef(
        name = "Item",
        minWidth = 150
      ),
      count = colDef(
        name = "Count",
        width = 80,
        style = function(value) {
          list(fontWeight = "600", color = color)
        }
      ),
      percent = colDef(
        name = "Percent",
        width = 100,
        format = colFormat(percent = TRUE, digits = 1),
        style = function(value) {
          list(
            background = paste0(color, "20"),
            borderLeft = paste0("4px solid ", color)
          )
        }
      )
    ),
    defaultPageSize = 20,
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    style = list(fontSize = "14px")
  )
}

# Create combined table with both genders side-by-side
create_combined_table <- function(data) {
  
  # Pivot to wide format
  wide_data <- data |>
    select(rank, gender_clean, clean_item_name, count, percent) |>
    pivot_wider(
      names_from = gender_clean,
      values_from = c(clean_item_name, count, percent),
      names_glue = "{gender_clean}_{.value}"
    ) |>
    select(
      rank,
      boy_clean_item_name, boy_count, boy_percent,
      girl_clean_item_name, girl_count, girl_percent
    )
  
  reactable(
    wide_data,
    columns = list(
      rank = colDef(
        name = "Rank",
        width = 60,
        style = list(fontWeight = "bold", textAlign = "center")
      ),
      boy_clean_item_name = colDef(
        name = "Item",
        minWidth = 150,
        header = function() {
          div(style = "color: #0D677C; font-weight: bold;", "Boys - Item")
        }
      ),
      boy_count = colDef(
        name = "Count",
        width = 80,
        style = list(color = "#0D677C", fontWeight = "600"),
        header = function() {
          div(style = "color: #0D677C; font-weight: bold;", "Count")
        }
      ),
      boy_percent = colDef(
        name = "Percent",
        width = 100,
        format = colFormat(percent = TRUE, digits = 1),
        style = function(value) {
          list(
            background = "#0D677C20",
            borderLeft = "4px solid #0D677C"
          )
        },
        header = function() {
          div(style = "color: #0D677C; font-weight: bold;", "Percent")
        }
      ),
      girl_clean_item_name = colDef(
        name = "Item",
        minWidth = 150,
        header = function() {
          div(style = "color: #C06C84; font-weight: bold;", "Girls - Item")
        }
      ),
      girl_count = colDef(
        name = "Count",
        width = 80,
        style = list(color = "#C06C84", fontWeight = "600"),
        header = function() {
          div(style = "color: #C06C84; font-weight: bold;", "Count")
        }
      ),
      girl_percent = colDef(
        name = "Percent",
        width = 100,
        format = colFormat(percent = TRUE, digits = 1),
        style = function(value) {
          list(
            background = "#C06C8420",
            borderLeft = "4px solid #C06C84"
          )
        },
        header = function() {
          div(style = "color: #C06C84; font-weight: bold;", "Percent")
        }
      )
    ),
    columnGroups = list(
      colGroup(name = "Boys", columns = c("boy_clean_item_name", "boy_count", "boy_percent"),
               headerStyle = list(background = "#0D677C20", fontWeight = "bold")),
      colGroup(name = "Girls", columns = c("girl_clean_item_name", "girl_count", "girl_percent"),
               headerStyle = list(background = "#C06C8420", fontWeight = "bold"))
    ),
    defaultPageSize = 20,
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    style = list(fontSize = "14px")
  )
}

# ============================================================================
# AGE COMPARISON - Functions for age_comparison.R
# ============================================================================

# Compute age trends for multiple items
compute_age_trends_multi <- function(filtered_data, full_data, items, genders, age_range) {
  
  # Get total participants per age and gender
  totals <- full_data |>
    filter(
      gender_clean %in% genders,
      age_num >= age_range[1],
      age_num <= age_range[2]
    ) |>
    group_by(age_num, gender_clean) |>
    summarize(total = n_distinct(participant_id), .groups = "drop")
  
  # Calculate proportions for each item
  filtered_data |>
    group_by(clean_item_name, age_num, gender_clean) |>
    summarize(count = n_distinct(participant_id), .groups = "drop") |>
    left_join(totals, by = c("age_num", "gender_clean")) |>
    mutate(percent = count / total) |>
    filter(!is.na(percent))
}

# Enhanced age trend plotting
plot_age_trends_enhanced <- function(df, view_mode, show_points, show_smooth, 
                                     line_size, selected_genders) {
  
  # Color palettes
  gender_colors <- c("girl" = "#C06C84", "boy" = "#0D677C")
  item_colors <- c("#2A7FFF", "#FF6B6B", "#4ECDC4", "#FFD93D", "#95E1D3", "#F38181")
  
  # Base plot
  if (view_mode == "facet_item") {
    # Facet by item
    p <- ggplot(df, aes(x = age_num, y = percent, 
                        color = gender_clean,
                        group = gender_clean))
    
  } else {
    # Facet by gender
    p <- ggplot(df, aes(x = age_num, y = percent, 
                        color = clean_item_name,
                        group = clean_item_name))
  }
  
  # Add lines
  p <- p + geom_line(size = line_size, alpha = 0.8)
  
  # Add points
  p <- p + geom_point(size = line_size * 1.5, alpha = 0.7)
  
  # Add facets if needed
  if (view_mode == "facet_item") {
    p <- p + facet_wrap(~clean_item_name, ncol = 2)
  } else if (view_mode == "facet_gender") {
    p <- p + facet_wrap(~gender_clean, ncol = 2)
  }
  
  # Theme and labels
  p <- p +
    labs(
      title = "Age Trends Comparison",
      x = "Age",
      y = "Proportion of Participants"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = seq(0, 20, 2)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 15)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray98", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "gray90", color = NA)
    )
  
  return(p)
}

# Compute summary statistics for trends
compute_trend_summary <- function(df) {
  df |>
    group_by(clean_item_name, gender_clean) |>
    summarize(
      `Min Age` = min(age_num, na.rm = TRUE),
      `Max Age` = max(age_num, na.rm = TRUE),
      `Avg Proportion` = scales::percent(mean(percent, na.rm = TRUE), accuracy = 0.1),
      `Peak Age` = age_num[which.max(percent)],
      `Peak Proportion` = scales::percent(max(percent, na.rm = TRUE), accuracy = 0.1),
      `Sample Size` = sum(count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(
      Item = clean_item_name,
      Gender = gender_clean
    )
}

# ============================================================================
# AGE BIN COMPARISON - Functions for age_bin_comparison.R
# ============================================================================

# Compute age bin summary (groups ages into bins to reduce noise)
compute_age_bin_summary <- function(data, item_name, genders) {
  
  # Filter by item and gender
  item_data <- data |>
    filter(
      clean_item_name == item_name,
      gender_clean %in% genders
    )
  
  # Get total participants per age bin and gender
  total_per_bin <- data |>
    filter(gender_clean %in% genders) |>
    group_by(age_bin, gender_clean) |>
    summarize(total = n_distinct(participant_id), .groups = "drop")
  
  # Calculate proportions
  result <- item_data |>
    group_by(age_bin, gender_clean) |>
    summarize(count = n_distinct(participant_id), .groups = "drop") |>
    left_join(total_per_bin, by = c("age_bin", "gender_clean")) |>
    mutate(percent = count / total) |>
    filter(!is.na(age_bin))
  
  # Get unique age bins and sort them by the starting age number
  age_bin_order <- result |>
    distinct(age_bin) |>
    mutate(start_age = as.numeric(str_extract(age_bin, "\\d+"))) |>
    arrange(start_age) |>
    pull(age_bin)
  
  # Convert age_bin to ordered factor
  result |>
    mutate(age_bin = factor(age_bin, levels = age_bin_order))
}

# Plot age bin trends (single plot, can show one or both genders)
plot_age_bin_trend <- function(df, item_name, gender_colors, selected_genders) {
  
  # If both genders selected, show lines for each
  if (length(selected_genders) == 2) {
    p <- ggplot(df, aes(x = age_bin, y = percent, color = gender_clean, group = gender_clean)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = gender_colors, name = "Gender")
    
    # If one gender, show single line
  } else {
    color_val <- gender_colors[selected_genders]
    p <- ggplot(df, aes(x = age_bin, y = percent, group = 1)) +
      geom_line(size = 1.2, color = color_val) +
      geom_point(size = 3, color = color_val)
  }
  
  # Add common elements
  p <- p +
    labs(
      title = paste("Age Bin Trends for:", item_name),
      x = "Age Group", 
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_discrete(labels = function(x) str_replace(x, " to ", "-")) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(p)
}

# Alternative: Plot age bin trends with faceted panels by gender
plot_age_bin_trend_faceted <- function(df, item_name) {
  ggplot(df, aes(x = age_bin, y = percent, group = 1)) +
    geom_line(size = 1.2, color = "#2A7FFF") +
    geom_point(size = 3, color = "#2A7FFF") +
    facet_wrap(~gender_clean, ncol = 2) +
    labs(
      title = paste("Age Bin Trends for:", item_name),
      x = "Age Bin",
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12, face = "bold")
    )
}

# ============================================================================
# ALIGNMENT - Functions for alignment.R
# ============================================================================

# Compute gender proportions for alignment visualization
compute_gender_proportions <- function(data) {
  data |>
    group_by(gender_clean, clean_item_name) |>
    summarize(count = n(), .groups = "drop") |>
    group_by(gender_clean) |>
    mutate(proportion = count / sum(count)) |>
    ungroup()
}

# Get top N items for each gender
get_top_items_by_gender <- function(df, n) {
  df |>
    group_by(gender_clean) |>
    slice_max(order_by = proportion, n = n, with_ties = FALSE) |>
    ungroup()
}

# Join boys and girls data to find shared items
join_shared_items <- function(boys_df, girls_df) {
  inner_join(
    boys_df |> select(clean_item_name, prop_boy = proportion),
    girls_df |> select(clean_item_name, prop_girl = proportion),
    by = "clean_item_name"
  )
}

# Count total participants by gender
count_participants_by_gender <- function(data) {
  data |>
    group_by(gender_clean) |>
    summarize(total = n_distinct(participant_id), .groups = "drop")
}

# Plot alignment visualization (slope graph)
plot_alignment <- function(boys_df, girls_df, shared_df) {
  
  ggplot() +
    geom_segment(
      data = shared_df,
      aes(x = 1, xend = 2, y = prop_boy, yend = prop_girl),
      color = "gray60"
    ) +
    geom_point(
      data = boys_df,
      aes(x = 1, y = proportion, size = proportion),
      color = "#0D677C", alpha = 0.8
    ) +
    geom_text_repel(
      data = boys_df,
      aes(x = 1, y = proportion, label = paste0(clean_item_name, " (", round(proportion*100,1), "%)")),
      nudge_x = -0.1,
      direction = "y",
      hjust = 1,
      size = 3.5,
      color = "#0D677C",
      segment.color = NA
    ) +
    geom_point(
      data = girls_df,
      aes(x = 2, y = proportion, size = proportion),
      color = "#C06C84", alpha = 0.8
    ) +
    geom_text_repel(
      data = girls_df,
      aes(x = 2, y = proportion, label = paste0(clean_item_name, " (", round(proportion*100,1), "%)")),
      nudge_x = 0.1,
      direction = "y",
      hjust = 0,
      size = 3.5,
      color = "#C06C84",
      segment.color = NA
    ) +
    scale_x_continuous(
      limits = c(0.5, 2.5),
      breaks = c(1, 2),
      labels = c("Boys", "Girls")
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Proportion of Participants", size = "Proportion") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
}

# ============================================================================
# INTEREST - Functions for interest.R (freelist data)
# ============================================================================

# Compute interest summary across four dimensions
compute_interest_summary <- function(data, collection_name, genders, age_range) {
  data |>
    filter(
      collection_name_matched == collection_name,
      gender_clean %in% genders,
      age_num >= age_range[1],
      age_num <= age_range[2]
    ) |>
    select(interest_learning, interest_curious, interest_talking, interest_playing) |>
    summarise(
      Learning = mean(interest_learning, na.rm = TRUE),
      Curious = mean(interest_curious, na.rm = TRUE),
      Talking = mean(interest_talking, na.rm = TRUE),
      Playing = mean(interest_playing, na.rm = TRUE),
      n = n()
    ) |>
    pivot_longer(
      cols = c(Learning, Curious, Talking, Playing),
      names_to = "dimension",
      values_to = "mean_score"
    )
}

# Plot interest profile line chart
plot_interest_profile <- function(summary_df, collection_name) {
  ggplot(summary_df, aes(x = dimension, y = mean_score, group = 1)) +
    geom_line(size = 1.2, color = "#2A7FFF") +
    geom_point(size = 4, color = "#2A7FFF") +
    scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
    labs(
      x = NULL,
      y = "Mean Agreement (1–7)",
      title = paste("Interest Profile for:", collection_name)
    ) +
    theme_minimal(base_size = 14)
}