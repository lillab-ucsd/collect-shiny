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

# Parse gender selection from dropdown
parse_gender_selection <- function(gender_filter) {
  if (gender_filter %in% c("boy", "girl")) {
    list(genders = gender_filter, separate = FALSE)
  } else if (gender_filter == "both_separate") {
    list(genders = c("boy", "girl"), separate = TRUE)
  } else {  # both_combined
    list(genders = c("boy", "girl"), separate = FALSE)
  }
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
plot_top_items <- function(df, title, color) {
  ggplot(df, aes(x = reorder(clean_item_name, percent), y = percent)) +
    geom_col(fill = color, width = 0.7, alpha = 0.9) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Proportion of Participants",
      title = title
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
      legend.title = element_text(face = "bold", size = 16),
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

# Compute age trends with combined gender option
compute_age_trends_combined <- function(filtered_data, full_data, items, age_range) {
  # Get total participants per age (combining both genders)
  totals <- full_data |>
    filter(
      age_num >= age_range[1],
      age_num <= age_range[2]
    ) |>
    group_by(age_num) |>
    summarize(total = n_distinct(participant_id), .groups = "drop")
  
  # Calculate proportions for each item (combining both genders)
  filtered_data |>
    group_by(clean_item_name, age_num) |>
    summarize(count = n_distinct(participant_id), .groups = "drop") |>
    left_join(totals, by = "age_num") |>
    mutate(
      percent = count / total,
      gender_clean = "combined"  # Add a gender column for consistency
    ) |>
    filter(!is.na(percent))
}

# Enhanced age trend plotting
plot_age_trends_enhanced <- function(df, view_mode, show_points, gender_mode) {
  gender_colors <- c("girl" = "#C06C84", "boy" = "#0D677C", "combined" = "#4A90E2")
  
  # Determine if we're showing gender separation
  show_gender <- gender_mode == "both_separate"
  
  # Base plot setup
  if (view_mode == "together") {
    # All items on one plot
    if (show_gender) {
      # Show different colors for each item, facet by gender instead of linetype
      p <- ggplot(df, aes(x = age_num, y = percent, 
                          color = clean_item_name, 
                          group = clean_item_name))
    } else {
      # Just show different colors for each item
      p <- ggplot(df, aes(x = age_num, y = percent, 
                          color = clean_item_name, 
                          group = clean_item_name))
    }
  } else {
    # Separate panel for each item (facet_item)
    if (show_gender) {
      # Within each item panel, show boy/girl lines
      p <- ggplot(df, aes(x = age_num, y = percent, 
                          color = gender_clean, 
                          group = gender_clean))
    } else {
      # Single line per item panel - color by gender_clean which will be boy, girl, or combined
      p <- ggplot(df, aes(x = age_num, y = percent, 
                          color = gender_clean, 
                          group = 1))
    }
  }
  
  # Add lines
  p <- p + geom_line(size = 1, alpha = 0.8)
  
  # Add points if requested
  if (show_points) {
    p <- p + geom_point(size = 1 * 1.5, alpha = 0.7)
  }
  
  # Apply color scales
  if (view_mode == "together") {
    # Use Viridis discrete palette for items
    p <- p + scale_color_viridis_d(option = "D", name = "Item")
  } else {
    # facet_item mode - always color by gender (boy, girl, or combined)
    p <- p + scale_color_manual(values = gender_colors, name = "Gender", guide = "none")
  }
  
  # Add facets if needed
  if (view_mode == "facet_item") {
    p <- p + facet_wrap(~clean_item_name, ncol = 2)
  } else if (view_mode == "together" && show_gender) {
    # When showing all items together with gender separation, facet by gender
    p <- p + facet_wrap(~gender_clean, ncol = 2,
                        labeller = labeller(gender_clean = c("boy" = "Boys", "girl" = "Girls")))
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
compute_trend_summary <- function(df, gender_mode) {
  if (gender_mode == "both_combined") {
    # Don't show gender column when combined
    df |>
      group_by(clean_item_name) |>
      summarize(
        `Min Age` = min(age_num, na.rm = TRUE),
        `Max Age` = max(age_num, na.rm = TRUE),
        `Avg Proportion` = scales::percent(mean(percent, na.rm = TRUE), accuracy = 0.1),
        `Peak Age` = age_num[which.max(percent)],
        `Peak Proportion` = scales::percent(max(percent, na.rm = TRUE), accuracy = 0.1),
        `Sample Size` = sum(count, na.rm = TRUE),
        .groups = "drop"
      ) |>
      rename(Item = clean_item_name)
  } else {
    # Show gender column when separate or single gender
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
      rename(Item = clean_item_name, Gender = gender_clean)
  }
}

# ============================================================================
# ALIGNMENT - Functions for alignment.R
# ============================================================================

# Compute gender proportions for alignment visualization (with optional age filtering)
compute_gender_proportions <- function(data, min_age = NULL, max_age = NULL) {
  filtered_data <- data
  
  # Apply age filtering if specified
  if (!is.null(min_age) && !is.null(max_age)) {
    filtered_data <- filtered_data |>
      filter(age_num >= min_age & age_num <= max_age)
  }
  
  filtered_data |>
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

# Count total participants by gender (with optional age filtering)
count_participants_by_gender <- function(data, min_age = NULL, max_age = NULL) {
  filtered_data <- data
  
  # Apply age filtering if specified
  if (!is.null(min_age) && !is.null(max_age)) {
    filtered_data <- filtered_data |>
      filter(age_num >= min_age & age_num <= max_age)
  }
  
  filtered_data |>
    group_by(gender_clean) |>
    summarize(total = n_distinct(participant_id), .groups = "drop")
}

# Create age bins with whole number ages using cut_interval
create_age_bins <- function(data, n_bins) {
  min_age <- floor(min(data$age_num, na.rm = TRUE))
  max_age <- ceiling(max(data$age_num, na.rm = TRUE))
  
  # Create bins directly from the data
  data_with_bins <- data |>
    mutate(age_bin = cut_interval(age_num, 
                                  n = n_bins,
                                  labels = FALSE,
                                  right = TRUE))
  
  # Get the actual breaks used by cut_interval
  bin_info <- data_with_bins |>
    group_by(age_bin) |>
    summarize(
      min_age_in_bin = min(age_num, na.rm = TRUE),
      max_age_in_bin = max(age_num, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(age_bin)
  
  # Create clean whole-number ranges and labels
  ranges <- list()
  labels <- character(n_bins)
  breaks <- numeric(n_bins + 1)
  
  breaks[1] <- floor(bin_info$min_age_in_bin[1])
  
  for (i in 1:n_bins) {
    # Set range for filtering (inclusive on both ends)
    range_min <- if (i == 1) {
      floor(bin_info$min_age_in_bin[i])
    } else {
      ceiling(bin_info$min_age_in_bin[i])
    }
    
    range_max <- ceiling(bin_info$max_age_in_bin[i])
    
    ranges[[i]] <- c(range_min, range_max)
    breaks[i + 1] <- range_max
    
    # Create labels (show as inclusive ranges, but filter appropriately)
    if (i == n_bins) {
      # Last bin includes the endpoint
      labels[i] <- paste0(range_min, "-", range_max)
    } else {
      # Other bins: label shows one less than the next bin's start
      next_start <- ceiling(bin_info$min_age_in_bin[i + 1])
      labels[i] <- paste0(range_min, "-", next_start - 1)
    }
  }
  
  # Return list with breaks and labels
  list(
    breaks = unique(breaks),
    labels = labels,
    ranges = ranges
  )
}

# Rank-ordered alignment plot with sample sizes (single age range)
plot_alignment_ranked <- function(boys_df, girls_df, shared_df, n_boys, n_girls, 
                                  age_label = NULL, n_bins = 1) {
  
  # Add ranks (rank 1 = highest proportion)
  boys_df <- boys_df |>
    arrange(desc(proportion)) |>
    mutate(rank = row_number())
  
  girls_df <- girls_df |>
    arrange(desc(proportion)) |>
    mutate(rank = row_number())
  
  # Update shared items with ranks
  shared_df <- shared_df |>
    left_join(boys_df |> select(clean_item_name, boy_rank = rank), by = "clean_item_name") |>
    left_join(girls_df |> select(clean_item_name, girl_rank = rank), by = "clean_item_name")
  
  # Create title with age label if provided
  plot_title <- if (!is.null(age_label)) {
    paste0("Ages ", age_label)
  } else {
    "Top Items by Gender (Rank Ordered)"
  }
  
  # Scale text and point sizes based on number of bins
  # More bins = smaller text and points to fit better
  # Less aggressive scaling to keep text readable
  scale_factor <- case_when(
    n_bins == 1 ~ 1.0,
    n_bins == 2 ~ 0.90,
    n_bins == 3 ~ 0.80,
    n_bins == 4 ~ 0.72,
    TRUE ~ 0.65  # 5+ bins
  )
  
  # Adjusted sizes based on scale factor (increased base sizes)
  item_label_size <- 4.0 * scale_factor  # increased from 3.5
  title_size <- 15 * scale_factor  # increased from 14
  sample_size_text <- 5.0 * scale_factor  # increased from 4.5
  axis_text_size <- 13 * scale_factor  # increased from 12
  
  # Scale point size range (make circles smaller with more bins, but less aggressive)
  point_size_min <- 3.5 * scale_factor
  point_size_max <- 10 * scale_factor
  
  # Adjust horizontal spacing for text labels (push text further from points)
  label_offset <- case_when(
    n_bins == 1 ~ 0.08,
    n_bins == 2 ~ 0.10,
    n_bins == 3 ~ 0.12,
    n_bins == 4 ~ 0.14,
    TRUE ~ 0.16  # 5+ bins
  )
  
  # Create plot
  ggplot() +
    # Connecting lines for shared items
    geom_segment(
      data = shared_df,
      aes(x = 1, xend = 2, y = boy_rank, yend = girl_rank),
      color = "gray60",
      alpha = 0.5,
      size = 0.8 * scale_factor
    ) +
    # Boys points (size based on proportion)
    geom_point(
      data = boys_df,
      aes(x = 1, y = rank, size = proportion),
      color = "#0D677C",
      alpha = 0.8
    ) +
    # Boys labels (aligned on left, pushed further from points)
    geom_text(
      data = boys_df,
      aes(x = 1 - label_offset, y = rank, 
          label = paste0(rank, ". ", clean_item_name, " (", round(proportion*100, 1), "%)")),
      hjust = 1,
      size = item_label_size,
      color = "#0D677C"
    ) +
    # Girls points (size based on proportion)
    geom_point(
      data = girls_df,
      aes(x = 2, y = rank, size = proportion),
      color = "#C06C84",
      alpha = 0.8
    ) +
    # Girls labels (aligned on right, pushed further from points)
    geom_text(
      data = girls_df,
      aes(x = 2 + label_offset, y = rank, 
          label = paste0(rank, ". ", clean_item_name, " (", round(proportion*100, 1), "%)")),
      hjust = 0,
      size = item_label_size,
      color = "#C06C84"
    ) +
    # Sample sizes at bottom (with more space)
    annotate(
      "text",
      x = 1,
      y = max(c(boys_df$rank, girls_df$rank)) + 2,
      label = paste0("n = ", n_boys),
      size = sample_size_text,
      fontface = "bold",
      color = "#0D677C"
    ) +
    annotate(
      "text",
      x = 2,
      y = max(c(boys_df$rank, girls_df$rank)) + 2,
      label = paste0("n = ", n_girls),
      size = sample_size_text,
      fontface = "bold",
      color = "#C06C84"
    ) +
    scale_x_continuous(
      limits = c(0.2, 2.8),
      breaks = c(1, 2),
      labels = c("Boys", "Girls")
    ) +
    scale_y_reverse(expand = expansion(mult = c(0.05, 0.15))) +  # Increased vertical expansion for better spacing
    scale_size_continuous(range = c(point_size_min, point_size_max), name = "Proportion") +
    labs(
      x = NULL,
      y = NULL,
      title = plot_title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",  # Remove legend to save space with multiple bins
      plot.title = element_text(face = "bold", size = title_size, hjust = 0.5, 
                                margin = margin(b = 5)),
      axis.text.x = element_text(face = "bold", size = axis_text_size, color = "gray30"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray98", color = NA),
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5)  # Adjust margins
    )
}

# Plot multiple age bins side by side or in a grid
plot_alignment_age_bins <- function(data, n_items, n_bins) {
  
  # Create automatic bins
  bin_info <- create_age_bins(data, n_bins)
  age_ranges <- bin_info$ranges
  age_labels <- bin_info$labels
  
  # Create a plot for each age bin
  plots <- list()
  
  for (i in seq_along(age_ranges)) {
    age_range <- age_ranges[[i]]
    age_label <- age_labels[i]
    
    # Filter data for this age range
    df_props <- compute_gender_proportions(data, age_range[1], age_range[2])
    
    # Get top items
    top_items <- get_top_items_by_gender(df_props, n_items)
    
    # Split by gender
    boys_df <- top_items |> filter(gender_clean == "boy")
    girls_df <- top_items |> filter(gender_clean == "girl")
    
    # Get shared items
    shared_df <- join_shared_items(boys_df, girls_df)
    
    # Get sample sizes
    totals <- count_participants_by_gender(data, age_range[1], age_range[2])
    n_boys <- totals |> filter(gender_clean == "boy") |> pull(total)
    n_girls <- totals |> filter(gender_clean == "girl") |> pull(total)
    
    # Create plot for this bin, passing n_bins for scaling
    plots[[i]] <- plot_alignment_ranked(boys_df, girls_df, shared_df, 
                                        n_boys, n_girls, age_label, n_bins)
  }
  
  # Determine layout based on number of bins
  # Use vertical space more efficiently
  if (n_bins == 2) {
    # 2 plots side by side
    combined_plot <- wrap_plots(plots, ncol = 2)
  } else if (n_bins == 3) {
    # 3 plots: 2 on top, 1 on bottom (or all vertical)
    combined_plot <- wrap_plots(plots, ncol = 2)
  } else if (n_bins == 4) {
    # 4 plots: 2x2 grid
    combined_plot <- wrap_plots(plots, ncol = 2)
  } else {
    # 5 plots: 2x3 grid (2 on top, 2 in middle, 1 on bottom)
    combined_plot <- wrap_plots(plots, ncol = 2)
  }
  
  return(combined_plot)
}

# ============================================================================
# INTEREST - Functions for interest.R (freelist data)
# ============================================================================

# Get selection choices based on type
get_selection_choices <- function(data, selection_type) {
  if (selection_type == "item") {
    data |>
      filter(!is.na(unified_name)) |>
      pull(unified_name) |>
      unique() |>
      sort()
  } else if (selection_type == "category") {
    data |>
      filter(!is.na(category)) |>
      pull(category) |>
      unique() |>
      sort()
  } else if (selection_type == "object_type") {
    data |>
      filter(!is.na(object_type)) |>
      pull(object_type) |>
      unique() |>
      sort()
  } else if (selection_type == "taxonomic_content") {
    data |>
      filter(!is.na(taxonomic_content), taxonomic_content != "NA") |>
      pull(taxonomic_content) |>
      unique() |>
      sort()
  } else if (selection_type == "natural") {
    data |>
      filter(!is.na(natural)) |>
      pull(natural) |>
      unique() |>
      sort()
  } else if (selection_type == "animal_related") {
    c("Yes" = "y", "No" = "no")
  } else if (selection_type == "fiction_related") {
    c("Yes" = "y", "No" = "no")
  }
}

# Get label for selection dropdown
get_selection_label <- function(selection_type) {
  labels <- c(
    "item" = "Select Item:",
    "category" = "Select Category:",
    "object_type" = "Select Object Type:",
    "taxonomic_content" = "Select Taxonomic Content:",
    "natural" = "Select Natural:",
    "animal_related" = "Animal-Related:",
    "fiction_related" = "Fiction-Related:"
  )
  labels[selection_type]
}

# Get items in a group
get_items_in_group <- function(data, selection_type, selection_value) {
  if (selection_type == "category") {
    data |>
      filter(category == selection_value) |>
      pull(unified_name) |>
      unique() |>
      sort()
  } else if (selection_type == "object_type") {
    data |>
      filter(object_type == selection_value) |>
      pull(unified_name) |>
      unique() |>
      sort()
  } else if (selection_type == "taxonomic_content") {
    data |>
      filter(taxonomic_content == selection_value) |>
      pull(unified_name) |>
      unique() |>
      sort()
  } else if (selection_type == "natural") {
    data |>
      filter(natural == selection_value) |>
      pull(unified_name) |>
      unique() |>
      sort()
  } else if (selection_type == "animal_related") {
    # Handle both "y" and "no" (NA) values
    if (selection_value == "no") {
      data |>
        filter(is.na(`animal-related`)) |>
        pull(unified_name) |>
        unique() |>
        sort()
    } else {
      data |>
        filter(`animal-related` == selection_value) |>
        pull(unified_name) |>
        unique() |>
        sort()
    }
  } else if (selection_type == "fiction_related") {
    # Handle both "y" and "no" (NA) values
    if (selection_value == "no") {
      data |>
        filter(is.na(`fiction-related`)) |>
        pull(unified_name) |>
        unique() |>
        sort()
    } else {
      data |>
        filter(`fiction-related` == selection_value) |>
        pull(unified_name) |>
        unique() |>
        sort()
    }
  }
}

# Compute interest summary for individual item
compute_interest_summary_item <- function(data, item_name, genders, age_range, gender_mode) {
  
  filtered <- data |>
    filter(
      unified_name == item_name,
      gender_clean %in% genders,
      age_num >= age_range[1],
      age_num <= age_range[2]
    )
  
  if (gender_mode == "both_separate") {
    # Show by gender - count DISTINCT participants per gender
    summary_data <- filtered |>
      group_by(gender_clean) |>
      summarise(
        Learning = mean(interest_learning, na.rm = TRUE),
        Curious = mean(interest_curious, na.rm = TRUE),
        Talking = mean(interest_talking, na.rm = TRUE),
        Playing = mean(interest_playing, na.rm = TRUE),
        n = n_distinct(response_id),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = c(Learning, Curious, Talking, Playing),
        names_to = "dimension",
        values_to = "mean_score"
      )
    
    # Add sample size information as an attribute
    n_by_gender <- filtered |>
      group_by(gender_clean) |>
      summarise(n = n_distinct(response_id), .groups = "drop")
    
    attr(summary_data, "sample_sizes") <- n_by_gender
    
    return(summary_data)
  } else {
    # Combined - count total distinct participants
    summary_data <- filtered |>
      summarise(
        Learning = mean(interest_learning, na.rm = TRUE),
        Curious = mean(interest_curious, na.rm = TRUE),
        Talking = mean(interest_talking, na.rm = TRUE),
        Playing = mean(interest_playing, na.rm = TRUE),
        n = n_distinct(response_id)
      ) |>
      pivot_longer(
        cols = c(Learning, Curious, Talking, Playing),
        names_to = "dimension",
        values_to = "mean_score"
      )
    
    # Add sample size as an attribute
    attr(summary_data, "sample_size") <- filtered |> 
      summarise(n = n_distinct(response_id)) |> 
      pull(n)
    
    return(summary_data)
  }
}

# Compute interest summary for grouped items
compute_interest_summary_grouped <- function(data, selection_type, selection_value, 
                                             genders, age_range, gender_mode) {
  
  # Filter by the dictionary field
  if (selection_type == "category") {
    filtered <- data |> filter(category == selection_value)
  } else if (selection_type == "object_type") {
    filtered <- data |> filter(object_type == selection_value)
  } else if (selection_type == "taxonomic_content") {
    filtered <- data |> filter(taxonomic_content == selection_value)
  } else if (selection_type == "natural") {
    filtered <- data |> filter(natural == selection_value)
  } else if (selection_type == "animal_related") {
    # Handle both "y" and "no" (NA) values
    if (selection_value == "no") {
      filtered <- data |> filter(is.na(`animal-related`))
    } else {
      filtered <- data |> filter(`animal-related` == selection_value)
    }
  } else if (selection_type == "fiction_related") {
    # Handle both "y" and "no" (NA) values
    if (selection_value == "no") {
      filtered <- data |> filter(is.na(`fiction-related`))
    } else {
      filtered <- data |> filter(`fiction-related` == selection_value)
    }
  }
  
  # Further filter by gender and age
  filtered <- filtered |>
    filter(
      gender_clean %in% genders,
      age_num >= age_range[1],
      age_num <= age_range[2]
    )
  
  if (gender_mode == "both_separate") {
    # Show by gender - count DISTINCT participants per gender
    summary_data <- filtered |>
      group_by(gender_clean) |>
      summarise(
        Learning = mean(interest_learning, na.rm = TRUE),
        Curious = mean(interest_curious, na.rm = TRUE),
        Talking = mean(interest_talking, na.rm = TRUE),
        Playing = mean(interest_playing, na.rm = TRUE),
        n = n_distinct(response_id),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = c(Learning, Curious, Talking, Playing),
        names_to = "dimension",
        values_to = "mean_score"
      )
    
    # Add sample size information as an attribute
    n_by_gender <- filtered |>
      group_by(gender_clean) |>
      summarise(n = n_distinct(response_id), .groups = "drop")
    
    attr(summary_data, "sample_sizes") <- n_by_gender
    
    return(summary_data)
  } else {
    # Combined - count total distinct participants
    summary_data <- filtered |>
      summarise(
        Learning = mean(interest_learning, na.rm = TRUE),
        Curious = mean(interest_curious, na.rm = TRUE),
        Talking = mean(interest_talking, na.rm = TRUE),
        Playing = mean(interest_playing, na.rm = TRUE),
        n = n_distinct(response_id)
      ) |>
      pivot_longer(
        cols = c(Learning, Curious, Talking, Playing),
        names_to = "dimension",
        values_to = "mean_score"
      )
    
    # Add sample size as an attribute
    attr(summary_data, "sample_size") <- filtered |> 
      summarise(n = n_distinct(response_id)) |> 
      pull(n)
    
    return(summary_data)
  }
}

# Enhanced plot function
plot_interest_profile_enhanced <- function(summary_df, selection_name, 
                                           selection_type, gender_mode) {
  
  gender_colors <- c("girl" = "#C06C84", "boy" = "#0D677C")
  
  # Create title based on selection type
  title_text <- if (selection_type == "item") {
    paste("Interest Profile for:", selection_name)
  } else if (selection_type == "category") {
    paste("Interest Profile for Category:", selection_name)
  } else if (selection_type == "object_type") {
    paste("Interest Profile for Object Type:", selection_name)
  } else if (selection_type == "taxonomic_content") {
    paste("Interest Profile for Taxonomic Content:", selection_name)
  } else if (selection_type == "natural") {
    paste("Interest Profile for Natural:", selection_name)
  } else if (selection_type == "animal_related") {
    paste("Interest Profile for Animal-Related Items:", ifelse(selection_name == "no", "No", "Yes"))
  } else if (selection_type == "fiction_related") {
    paste("Interest Profile for Fiction-Related Items:", ifelse(selection_name == "no", "No", "Yes"))
  }
  
  # Create caption with sample size information
  caption_text <- NULL
  if (gender_mode == "both_separate") {
    # Get sample sizes from attribute
    sample_sizes <- attr(summary_df, "sample_sizes")
    if (!is.null(sample_sizes)) {
      boy_n <- sample_sizes |> filter(gender_clean == "boy") |> pull(n)
      girl_n <- sample_sizes |> filter(gender_clean == "girl") |> pull(n)
      
      if (length(boy_n) > 0 && length(girl_n) > 0) {
        caption_text <- paste0("Boys: n=", boy_n, " | Girls: n=", girl_n)
      } else if (length(boy_n) > 0) {
        caption_text <- paste0("Boys: n=", boy_n)
      } else if (length(girl_n) > 0) {
        caption_text <- paste0("Girls: n=", girl_n)
      }
    }
  } else {
    # Get combined sample size from attribute
    sample_size <- attr(summary_df, "sample_size")
    if (!is.null(sample_size)) {
      caption_text <- paste0("n=", sample_size)
    }
  }
  
  coord_cartesian(ylim = c(1, 7))
  
  # Create plot
  if (gender_mode == "both_separate" && "gender_clean" %in% names(summary_df)) {
    # Grouped bars for boys and girls
    p <- ggplot(summary_df, aes(x = dimension, y = mean_score, fill = gender_clean)) +
      geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
      scale_fill_manual(values = gender_colors, name = "Gender") +
      scale_y_continuous(breaks = 1:7, expand = expansion(mult = c(0, 0.05))) +
      coord_cartesian(ylim = c(1, 7)) +
      labs(
        x = NULL,
        y = "Mean Agreement (1–7)",
        title = title_text,
        caption = caption_text
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5, face = "bold", color = "gray20", margin = margin(t = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "gray98", color = NA)
      )
  } else {
    # Single bars - determine color based on gender_mode
    bar_color <- if (gender_mode == "boy") {
      "#0D677C"  # Boy color
    } else if (gender_mode == "girl") {
      "#C06C84"  # Girl color
    } else {
      "#4A90E2"  # Combined color (both_combined)
    }
    
    p <- ggplot(summary_df, aes(x = dimension, y = mean_score)) +
      geom_col(fill = bar_color, width = 0.6, alpha = 0.9) +
      scale_y_continuous(breaks = 1:7, expand = expansion(mult = c(0, 0.05))) +
      coord_cartesian(ylim = c(1, 7)) +
      labs(
        x = NULL,
        y = "Mean Agreement (1–7)",
        title = title_text,
        caption = caption_text
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5, face = "bold", color = "gray20", margin = margin(t = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "gray98", color = NA)
      )
  }
  
  return(p)
}