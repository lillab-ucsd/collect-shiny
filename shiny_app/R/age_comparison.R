# ============================================================================
# FILE: R/age_comparison.R
# PURPOSE: Compare age trends for multiple items with enhanced visualizations
# DESCRIPTION: Interactive age trend comparison with customizable display options,
#              multiple item selection, and aesthetic design improvements
# ============================================================================

age_comparison_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Age Comparison",
           fluidRow(
             # Left sidebar
             column(3,
                    wellPanel(
                      style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
                      h4("Item Selection", style = "font-weight: bold; color: #495057;"),
                      
                      selectInput(
                        ns("item_select"),
                        "Select Items to Compare:",
                        choices = NULL,
                        multiple = TRUE,
                        selectize = TRUE
                      ),
                      
                      helpText("Select up to 4 items to compare"),
                      
                      hr(),
                      
                      h4("Display Options", style = "font-weight: bold; color: #495057;"),
                      
                      checkboxGroupInput(
                        ns("gender_filter"),
                        "Gender:",
                        choices = c("boy", "girl"),
                        selected = c("boy", "girl")
                      ),
                      
                      sliderInput(
                        ns("age_range"),
                        "Age Range:",
                        min = 1,
                        max = 18,
                        value = c(1, 18),
                        step = 1
                      ),
                      
                      radioButtons(
                        ns("view_mode"),
                        "View Mode:",
                        choices = c(
                          "Facet by Item" = "facet_item",
                          "Facet by Gender" = "facet_gender"
                        ),
                        selected = "facet_item"
                      ),
                      
                      sliderInput(
                        ns("line_size"),
                        "Line Thickness:",
                        min = 0.5,
                        max = 3,
                        value = 1.2,
                        step = 0.1
                      )
                    )
             ),
             
             # Main plot area
             column(9,
                    wellPanel(
                      style = "background-color: white; border: 1px solid #dee2e6;",
                      plotOutput(ns("trend_plot"), height = "600px")
                    ),
                    
                    # Summary statistics
                    wellPanel(
                      style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
                      h4("Summary Statistics", style = "font-weight: bold; color: #495057;"),
                      tableOutput(ns("summary_table"))
                    )
             )
           ))
}

age_comparison_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update item choices and age range based on data
    observe({
      items <- sort(unique(data$clean_item_name))
      updateSelectInput(
        session,
        "item_select",
        choices = items,
        selected = items[1:2]  # Default to first 2 items
      )
      
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
    
    # Filter data
    filtered_data <- reactive({
      req(input$item_select)
      req(length(input$gender_filter) > 0)
      
      data |>
        filter(
          clean_item_name %in% input$item_select,
          gender_clean %in% input$gender_filter,
          age_num >= input$age_range[1],
          age_num <= input$age_range[2]
        )
    })
    
    # Compute age trends
    trend_data <- reactive({
      req(nrow(filtered_data()) > 0)
      
      compute_age_trends_multi(
        filtered_data(),
        data,
        input$item_select,
        input$gender_filter,
        input$age_range
      )
    })
    
    # Render main plot
    output$trend_plot <- renderPlot({
      req(nrow(trend_data()) > 0)
      
      plot_age_trends_enhanced(
        trend_data(),
        view_mode = input$view_mode,
        line_size = input$line_size,
        selected_genders = input$gender_filter
      )
    })
    
    # Render summary table
    output$summary_table <- renderTable({
      req(nrow(trend_data()) > 0)
      
      compute_trend_summary(trend_data())
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}