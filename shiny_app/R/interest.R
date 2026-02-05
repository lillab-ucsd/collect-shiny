# ============================================================================
# FILE: R/interest.R
# PURPOSE: Interest profiles for freelist collections with dictionary grouping
# DESCRIPTION: Line plot showing mean interest ratings with options to view
#              by individual items or grouped by dictionary categories
# ============================================================================

interest_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Interest Profiles",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("selection_type"),
          "View By:",
          choices = c(
            "Individual Item" = "item",
            "Object Type" = "object_type",
            "Category" = "category",
            "Natural" = "natural",
            "Taxonomic Content" = "taxonomic_content",
            "Animal-Related" = "animal_related",
            "Fiction-Related" = "fiction_related"
          ),
          selected = "item"
        ),
        
        selectInput(
          ns("selection"),
          "Select:",
          choices = NULL
        ),
        
        hr(),
        
        sliderInput(
          ns("age_range"),
          "Age Range:",
          min = 1,
          max = 18,
          value = c(1, 18)
        ),
        
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
        
        textOutput(ns("sample_size"))
      ),
      mainPanel(
        plotOutput(ns("interest_plot"), height = "400px"),
        
        wellPanel(
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6; margin-top: 20px;",
          h4("Items Included", style = "font-weight: bold; color: #495057;"),
          textOutput(ns("items_included"))
        )
      )
    )
  )
}

interest_server <- function(id, freelist_long_data) {
  moduleServer(id, function(input, output, session) {
    
    # Update age range based on data
    observe({
      updateSliderInput(
        session,
        "age_range",
        min = min(freelist_long_data$age_num, na.rm = TRUE),
        max = max(freelist_long_data$age_num, na.rm = TRUE),
        value = c(min(freelist_long_data$age_num, na.rm = TRUE), 
                  max(freelist_long_data$age_num, na.rm = TRUE))
      )
    })
    
    # Update selection dropdown based on selection type
    observe({
      choices <- get_selection_choices(freelist_long_data, input$selection_type)
      
      updateSelectInput(
        session,
        "selection",
        label = get_selection_label(input$selection_type),
        choices = choices,
        selected = choices[1]
      )
    })
    
    # Compute summary based on selection type
    summary <- reactive({
      req(input$selection)
      
      gender_info <- parse_gender_selection(input$gender_filter)
      
      if (input$selection_type == "item") {
        # Individual item
        compute_interest_summary_item(
          freelist_long_data,
          input$selection,
          gender_info$genders,
          input$age_range,
          input$gender_filter
        )
      } else {
        # Grouped by dictionary field
        compute_interest_summary_grouped(
          freelist_long_data,
          input$selection_type,
          input$selection,
          gender_info$genders,
          input$age_range,
          input$gender_filter
        )
      }
    })
    
    # Get items included in selection
    items_list <- reactive({
      req(input$selection)
      
      if (input$selection_type == "item") {
        input$selection
      } else {
        get_items_in_group(freelist_long_data, input$selection_type, input$selection)
      }
    })
    
    # Display items included
    output$items_included <- renderText({
      items <- items_list()
      if (length(items) == 1) {
        paste0("Showing: ", items)
      } else {
        paste0("Showing ", length(items), " items: ", paste(items, collapse = ", "))
      }
    })
    
    # Render plot
    output$interest_plot <- renderPlot({
      req(nrow(summary()) > 0)
      
      plot_interest_profile_enhanced(
        summary(),
        input$selection,
        input$selection_type,
        input$gender_filter
      )
    })
  })
}