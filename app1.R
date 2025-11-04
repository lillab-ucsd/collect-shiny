library(shiny)
library(tidyverse)
library(here)
library(ggforce)
library(ggrepel)
library(reactable)


checklist_data <- read_csv(here("data/collect_v1_checklist_long.csv"))

# UI
ui <- navbarPage(
  title = "Checklist Data Explorer",
  
  # ---------------------------
  # TAB 1: Top Items
  # ---------------------------
  tabPanel("Top Items",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("gender_filter", "Select Gender:",
                                  choices = c("boy", "girl"), selected = c("boy", "girl")),
               
               sliderInput(
                 "age_range", 
                 "Select Age Range:",
                 min = min(checklist_data$age_num, na.rm = TRUE),
                 max = max(checklist_data$age_num, na.rm = TRUE),
                 value = c(min(checklist_data$age_num, na.rm = TRUE), max(checklist_data$age_num, na.rm = TRUE)),
                 step = 1
               ),
               
               sliderInput("top_n", "Top N items to show:",
                           min = 5, max = 40, value = 10, step = 1)
             ),
             
             mainPanel(
               plotOutput("bar_plot")
             )
           )
  ),
  
  # ---------------------------
  # TAB 2: Age Comparison (Two Items)
  # ---------------------------
  tabPanel("Age Comparison",
           sidebarLayout(
             sidebarPanel(
               selectInput("age_item1", "Select Item 1:",
                           choices = sort(unique(checklist_data$clean_item_name)),
                           selected = "action figures"),
               
               selectInput("age_item2", "Select Item 2:",
                           choices = sort(unique(checklist_data$clean_item_name)),
                           selected = "animal figurines")
             ),
             
             mainPanel(
               fluidRow(
                 column(6, plotOutput("age_plot1")),
                 column(6, plotOutput("age_plot2"))
               )
             )
           )
  ),
  
  # ---------------------------
  # TAB 3: Gender Alignment
  # ---------------------------
  tabPanel("Alignment Visualization",
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 "n_items_align", "Number of Top Items to Show:",
                 min = 5, max = 40, value = 10, step = 1
               )
             ),
             mainPanel(
               fluidRow(
                 column(6, textOutput("n_boys")),
                 column(6, textOutput("n_girls"))
               ),
               plotOutput("alignmentPlot", height = "800px")
             )
           )
  )

)

# Server
server <- function(input, output, session) {
  
  # ---------------------------
  # TAB 1: Top Items
  # ---------------------------
  filtered_data <- reactive({
    checklist_data |>
      filter(
        gender_clean %in% input$gender_filter,
        age_num >= input$age_range[1],
        age_num <= input$age_range[2]
      )
  })
  
  summary_data <- reactive({
    fd <- filtered_data()
    
    total_participants <- fd |>
      distinct(participant_id) |>
      nrow()
    
    fd |>
      group_by(clean_item_name) |>
      summarize(count = n_distinct(participant_id), .groups = "drop") |>
      mutate(percent = count / total_participants) |>
      arrange(desc(count)) |>
      slice_head(n = input$top_n)
  })
  
  output$bar_plot <- renderPlot({
    summary_data() |>
      ggplot(aes(x = reorder(clean_item_name, percent), y = percent)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Checklist Item", y = "Proportion of Participants") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  })
  
  
  # ---------------------------
  # TAB 2: Age Comparison (Two Items)
  # ---------------------------
  
  # helper function that calculates proportions by age group and gender
  # compute_age_summary <- function(item_name) {
  #   fd <- checklist_data |>
  #     filter(clean_item_name == item_name)
  #   
  #   total_per_age_gender <- checklist_data |>
  #     group_by(age_num, gender_clean) |>
  #     summarize(total = n_distinct(participant_id), .groups = "drop")
  #   
  #   fd |>
  #     group_by(age_num, gender_clean) |>
  #     summarize(count = n_distinct(participant_id), .groups = "drop") |>
  #     left_join(total_per_age_gender, by = c("age_num", "gender_clean")) |>
  #     mutate(percent = count / total)
  # }
  
  # helper function that does proportions by age group
  compute_age_summary <- function(item_name) {
    # Filter just for the selected item
    fd <- checklist_data |>
      filter(clean_item_name == item_name)
    
    # Total participants per age (both genders combined)
    total_per_age <- checklist_data |>
      group_by(age_num) |>
      summarize(total = n_distinct(participant_id), .groups = "drop")
    
    # Count how many participants of each age + gender liked the item
    fd |>
      group_by(age_num, gender_clean) |>
      summarize(count = n_distinct(participant_id), .groups = "drop") |>
      left_join(total_per_age, by = "age_num") |>
      mutate(percent = count / total)
  }
  
  
  # Reactive summaries for each item
  age_summary1 <- reactive({
    compute_age_summary(input$age_item1)
  })
  
  age_summary2 <- reactive({
    compute_age_summary(input$age_item2)
  })
  
  # Custom gender colors
  gender_colors <- c("girl" = "#C06C84", "boy" = "#0D677C")
  
  # --- Plot 1: Item 1 ---
  output$age_plot1 <- renderPlot({
    age_summary1() |>
      ggplot(aes(x = age_num, y = percent, color = gender_clean, group = gender_clean)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = gender_colors) +
      labs(
        title = paste("Trends for:", input$age_item1),
        x = "Age", y = "Proportion", color = "Gender"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # --- Plot 2: Item 2 ---
  output$age_plot2 <- renderPlot({
    age_summary2() |>
      ggplot(aes(x = age_num, y = percent, color = gender_clean, group = gender_clean)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = gender_colors) +
      labs(
        title = paste("Trends for:", input$age_item2),
        x = "Age", y = "Proportion", color = "Gender"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  
  # ---- Alignment Visualization ----
  output$alignmentPlot <- renderPlot({
    req(input$n_items_align)
    
    # ---- Calculate proportions by gender ----
    df <- checklist_data |>
      group_by(gender_clean, clean_item_name) |>
      summarize(count = n(), .groups = "drop") |>
      group_by(gender_clean) |>
      mutate(proportion = count / sum(count)) |>
      ungroup()
    
    # ---- Select top N items per gender ----
    top_items <- df |>
      group_by(gender_clean) |>
      slice_max(order_by = proportion, n = input$n_items_align, with_ties = FALSE) |>
      ungroup()
    
    boys_df <- top_items |> filter(gender_clean == "boy")
    girls_df <- top_items |> filter(gender_clean == "girl")
    
    # ---- Wide table for shared items ----
    shared <- inner_join(
      boys_df |> select(clean_item_name, prop_boy = proportion),
      girls_df |> select(clean_item_name, prop_girl = proportion),
      by = "clean_item_name"
    )
    
    # ---- Plot ----
    ggplot() +
      # connecting lines for shared items
      geom_segment(
        data = shared,
        aes(x = 1, xend = 2, y = prop_boy, yend = prop_girl),
        color = "gray60"
      ) +
      
      # boys' points
      geom_point(
        data = boys_df,
        aes(x = 1, y = proportion, size = proportion),
        color = "#0D677C", alpha = 0.8
      ) +
      geom_text_repel(
        data = boys_df,
        aes(x = 1, y = proportion, label = paste0(clean_item_name, " (", round(proportion*100,1), "%)")),
        nudge_x = -0.1, direction = "y", hjust = 1,
        size = 3.5, color = "#0D677C",
        segment.color = NA
      ) +
      
      # girls' points
      geom_point(
        data = girls_df,
        aes(x = 2, y = proportion, size = proportion),
        color = "#C06C84", alpha = 0.8
      ) +
      geom_text_repel(
        data = girls_df,
        aes(x = 2, y = proportion, label = paste0(clean_item_name, " (", round(proportion*100,1), "%)")),
        nudge_x = 0.1, direction = "y", hjust = 0,
        size = 3.5, color = "#C06C84",
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
  })
  
  totals <- reactive({
    checklist_data |>
      group_by(gender_clean) |>
      summarize(total = n_distinct(participant_id), .groups = "drop")
  })
  
  output$n_boys <- renderText({
    paste0("n (Boys) = ", totals() |> filter(gender_clean == "boy") |> pull(total))
  })
  
  output$n_girls <- renderText({
    paste0("n (Girls) = ", totals() |> filter(gender_clean == "girl") |> pull(total))
  })

  
}

shinyApp(ui = ui, server = server)





