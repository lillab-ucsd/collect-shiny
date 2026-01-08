# ============================================================================
# FILE: R/top_items_table.R
# PURPOSE: Interactive table showing top collected items by gender
# DESCRIPTION: Sortable, filterable reactable displaying top items with 
#              side-by-side or combined view, showing rank, count, and percent
# ============================================================================

top_items_table_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Top Items Table",
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 ns("n_items"),
                 "Number of Top Items to Show:",
                 min = 5,
                 max = 50,
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
               ),
               checkboxGroupInput(
                 ns("gender_filter"),
                 "Gender:",
                 choices = c("boy", "girl"),
                 selected = c("boy", "girl")
               ),
               hr(),
               radioButtons(
                 ns("view_type"),
                 "View Type:",
                 choices = c(
                   "Side by Side" = "side_by_side",
                   "Combined Table" = "combined"
                 ),
                 selected = "side_by_side"
               ),
               hr(),
               helpText("Shows the most commonly collected items by gender.")
             ),
             mainPanel(
               uiOutput(ns("table_output"))
             )
           ))
}

top_items_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update age range based on data
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
    
    # Filter data based on selections
    filtered_data <- reactive({
      data |>
        filter(
          gender_clean %in% input$gender_filter,
          age_num >= input$age_range[1],
          age_num <= input$age_range[2]
        )
    })
    
    # Compute top items by gender
    top_items_by_gender <- reactive({
      compute_top_items_by_gender(filtered_data(), input$n_items)
    })
    
    # Render appropriate view
    output$table_output <- renderUI({
      ns <- session$ns
      
      if (input$view_type == "side_by_side") {
        # Side by side tables
        boys_data <- top_items_by_gender() |> filter(gender_clean == "boy")
        girls_data <- top_items_by_gender() |> filter(gender_clean == "girl")
        
        fluidRow(
          column(
            6,
            h4("Boys", style = "color: #0D677C; text-align: center;"),
            reactableOutput(ns("boys_table"))
          ),
          column(
            6,
            h4("Girls", style = "color: #C06C84; text-align: center;"),
            reactableOutput(ns("girls_table"))
          )
        )
      } else {
        # Combined table
        reactableOutput(ns("combined_table"))
      }
    })
    
    # Boys table
    output$boys_table <- renderReactable({
      boys_data <- top_items_by_gender() |> filter(gender_clean == "boy")
      
      create_top_items_table(boys_data, "#0D677C")
    })
    
    # Girls table
    output$girls_table <- renderReactable({
      girls_data <- top_items_by_gender() |> filter(gender_clean == "girl")
      
      create_top_items_table(girls_data, "#C06C84")
    })
    
    # Combined table
    output$combined_table <- renderReactable({
      create_combined_table(top_items_by_gender())
    })
  })
}
