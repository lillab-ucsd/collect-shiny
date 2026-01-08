# ============================================================================
# FILE: R/interest.R
# PURPOSE: Interest profile visualization for freelist collections
# DESCRIPTION: Line plot showing mean interest ratings across four dimensions 
#              (learning, curious, talking, playing) for selected collections
# ============================================================================

interest_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Interest Profiles",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("collection"), "Select Collection:", choices = NULL),
        sliderInput(ns("age_range"), "Age Range:", 
                    min = 1, max = 18, value = c(1, 18)),
        checkboxGroupInput(
          ns("gender"),
          "Gender:",
          choices = c("boy", "girl"),
          selected = c("boy", "girl")
        ),
        hr(),
        textOutput(ns("sample_size"))
      ),
      mainPanel(
        plotOutput(ns("interest_plot"), height = "400px")
      )
    )
  )
}

interest_server <- function(id, freelist_long_data) {
  moduleServer(id, function(input, output, session) {
    
    # Update UI inputs based on data
    observe({
      collections <- freelist_long_data |>
        filter(!is.na(collection_name_matched)) |>
        pull(collection_name_matched) |>
        unique() |>
        sort()
      
      updateSelectInput(
        session,
        "collection",
        choices = collections,
        selected = collections[1]
      )
      
      updateSliderInput(
        session,
        "age_range",
        min = min(freelist_long_data$age_num, na.rm = TRUE),
        max = max(freelist_long_data$age_num, na.rm = TRUE),
        value = c(min(freelist_long_data$age_num, na.rm = TRUE), 
                  max(freelist_long_data$age_num, na.rm = TRUE))
      )
    })
    
    # Compute summary statistics
    summary <- reactive({
      req(input$collection)
      compute_interest_summary(
        freelist_long_data,
        input$collection,
        input$gender,
        input$age_range
      )
    })
    
    # Display sample size
    output$sample_size <- renderText({
      n <- summary() |> pull(n) |> first()
      paste0("Sample size: n = ", n)
    })
    
    # Render plot
    output$interest_plot <- renderPlot({
      req(nrow(summary()) > 0)
      plot_interest_profile(summary(), input$collection)
    })
  })
}