# ============================================================================
# FILE: R/age_bin_comparison.R
# PURPOSE: Age trend comparison using age bins to reduce noise
# DESCRIPTION: Compare two items across age groups (bins) with options to 
#              view boys, girls, or both, showing clearer developmental trends
# ============================================================================

age_bin_comparison_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Age Bin Comparison",
           sidebarLayout(
             sidebarPanel(
               selectInput(ns("bin_item1"), "Select Item 1:", choices = NULL),
               selectInput(ns("bin_item2"), "Select Item 2:", choices = NULL),
               checkboxGroupInput(
                 ns("gender_filter"),
                 "Gender:",
                 choices = c("boy", "girl"),
                 selected = c("boy", "girl")
               ),
               hr(),
               helpText("Age bins group similar ages together to reduce noise in the data.")
             ),
             mainPanel(
               fluidRow(
                 column(6, plotOutput(ns("bin_plot1"))),
                 column(6, plotOutput(ns("bin_plot2")))
               )
             )
           ))
}

age_bin_comparison_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update dropdown choices
    updateSelectInput(session, "bin_item1",
                      choices = sort(unique(data$clean_item_name)),
                      selected = "action figures")
    
    updateSelectInput(session, "bin_item2",
                      choices = sort(unique(data$clean_item_name)),
                      selected = "animal figurines")
    
    # Color palette
    gender_colors <- c("girl" = "#C06C84", "boy" = "#0D677C")
    
    # Compute summary for item 1
    summary1 <- reactive({
      compute_age_bin_summary(data, input$bin_item1, input$gender_filter)
    })
    
    # Compute summary for item 2
    summary2 <- reactive({
      compute_age_bin_summary(data, input$bin_item2, input$gender_filter)
    })
    
    # Plot for item 1
    output$bin_plot1 <- renderPlot({
      req(nrow(summary1()) > 0)
      plot_age_bin_trend(summary1(), input$bin_item1, gender_colors, input$gender_filter)
    })
    
    # Plot for item 2
    output$bin_plot2 <- renderPlot({
      req(nrow(summary2()) > 0)
      plot_age_bin_trend(summary2(), input$bin_item2, gender_colors, input$gender_filter)
    })
  })
}

