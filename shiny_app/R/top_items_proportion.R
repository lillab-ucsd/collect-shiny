# ============================================================================
# FILE: R/top_items_proportion.R
# PURPOSE: Bar chart showing proportion of participants with top items
# DESCRIPTION: Visualize most commonly collected items with filters for 
#              gender and age
# ============================================================================

top_items_proportion_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Top Items Proportion",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput(
                 ns("gender_filter"),
                 "Select Gender:",
                 choices = c("boy", "girl"),
                 selected = c("boy", "girl")
               ),
               sliderInput(
                 ns("age_range"),
                 "Select Age Range:",
                 min = 1,
                 max = 100,
                 value = c(1, 100),
                 step = 1
               ),
               sliderInput(
                 ns("top_n"),
                 "Top N items to show:",
                 min = 5,
                 max = 40,
                 value = 10,
                 step = 1
               ),
               hr(),
               radioButtons(
                 ns("color_by"),
                 "Color bars by:",
                 choices = c(
                   "Single Color" = "single",
                   "By Gender" = "gender"
                 ),
                 selected = "single"
               ),
               hr(),
               helpText("Shows the proportion of participants who collect each item.")
             ),
             mainPanel(
               plotOutput(ns("bar_plot"), height = "600px")
             )
           ))
}

top_items_proportion_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update age range slider based on actual data
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
    
    # Filter data based on user selections
    filtered <- reactive({
      filter_checklist(data, input$gender_filter, input$age_range)
    })
    
    # Compute summary statistics
    summary <- reactive({
      filtered() |>
        count_by_item() |>
        slice_max(percent, n = input$top_n)
    })
    
    # Render plot
    output$bar_plot <- renderPlot({
      req(nrow(summary()) > 0)
      
      if (input$color_by == "gender") {
        plot_top_items_by_gender(filtered(), input$top_n)
      } else {
        plot_top_items(summary())
      }
    })
  })
}