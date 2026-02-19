# ============================================================================
# FILE: R/top_items_proportion.R
# PURPOSE: Bar chart showing proportion of participants with top items
# DESCRIPTION: Visualize most commonly collected items with filters for 
#              gender and age
# ============================================================================

top_items_proportion_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Top Items",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 ns("gender_filter"),
                 "Select Gender:",
                 choices = c(
                   "Boys Only" = "boy",
                   "Girls Only" = "girl",
                   "Boys & Girls (Separated)" = "both_separate",
                   "Boys & Girls (Combined)" = "both_combined"
                 ),
                 selected = "both_combined"
               ),
               sliderInput(
                 ns("top_n"),
                 "Number of Top Items to Show:",
                 min = 5,
                 max = 40,
                 value = 10,
                 step = 5
               ),
               sliderInput(
                 ns("age_range"),
                 "Age Range:",
                 min = 1,
                 max = 18,
                 value = c(1, 18),
                 step = 1
               )
             ),
             mainPanel(
               plotOutput(ns("bar_plot"), height = "600px")
             )
           ))
}

top_items_proportion_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      min_age <- min(data$age_num, na.rm = TRUE)
      max_age <- max(data$age_num, na.rm = TRUE)
      updateSliderInput(
        session,
        "age_range",
        min = min_age,
        max = max_age,
        value = c(min_age, max_age)
      )
    })
    
    filtered <- reactive({
      # Determine which genders to include
      genders <- if (input$gender_filter %in% c("boy", "girl")) {
        input$gender_filter
      } else {
        c("boy", "girl")
      }
      
      filter_checklist(data, genders, input$age_range)
    })
    
    output$bar_plot <- renderPlot({
      req(nrow(filtered()) > 0)
      if (input$gender_filter == "both_separate") {
        plot_top_items_by_gender(filtered(), input$top_n)
      } else {
        summary <- filtered() |> 
          count_by_item() |> 
          slice_max(percent, n = input$top_n)
        title <- switch(
          input$gender_filter,
          "boy"  = "Top Collected Items – Boys",
          "girl" = "Top Collected Items – Girls",
          "both_combined" = "Top Collected Items"
        )
        color <- switch(
          input$gender_filter,
          "boy"  = "#0D677C",
          "girl" = "#C06C84",
          "both_combined" = "#4A90E2"
        )
        
        plot_top_items(summary, title, color)
      }
    })
  })
}
