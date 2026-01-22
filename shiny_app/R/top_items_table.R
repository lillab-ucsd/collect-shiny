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
                 ns("n_items"),
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
               uiOutput(ns("table_output"))
             )
           ))
}

top_items_table_server <- function(id, data) {
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
    
    filtered_data <- reactive({
      genders <- if (input$gender_filter %in% c("boy", "girl")) {
        input$gender_filter
      } else {
        c("boy", "girl")
      }
      
      data |>
        filter(
          gender_clean %in% genders,
          age_num >= input$age_range[1],
          age_num <= input$age_range[2]
        )
    })
    
    top_items_data <- reactive({
      if (input$gender_filter == "both_combined") {
        # Combined: ignore gender
        filtered_data() |>
          count_by_item() |>
          slice_max(percent, n = input$n_items) |>
          mutate(rank = row_number())
      } else {
        # By gender
        compute_top_items_by_gender(filtered_data(), input$n_items)
      }
    })
    
    output$table_output <- renderUI({
      ns <- session$ns
      
      if (input$gender_filter == "both_separate") {
        # Side by side tables
        boys_data <- top_items_data() |> filter(gender_clean == "boy")
        girls_data <- top_items_data() |> filter(gender_clean == "girl")
        
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
      } else if (input$gender_filter == "both_combined") {
        tagList(
          h3(
            "Top Collected Items",
            style = "text-align: center; margin-bottom: 10px; color: #4A90E2;"
          ),
          reactableOutput(ns("single_table"))
        )
        
      } else if (input$gender_filter == "boy") {
        tagList(
          h3(
            "Top Collected Items – Boys",
            style = "text-align: center; margin-bottom: 10px; color: #0D677C;"
          ),
          reactableOutput(ns("single_table"))
        )
        
      } else if (input$gender_filter == "girl") {
        tagList(
          h3(
            "Top Colleted Items – Girls",
            style = "text-align: center; margin-bottom: 10px; color: #C06C84;"
          ),
          reactableOutput(ns("single_table"))
        )
      }
    })
    
    output$boys_table <- renderReactable({
      boys_data <- top_items_data() |> filter(gender_clean == "boy")
      create_top_items_table(boys_data, "#0D677C")
    })
    
    output$girls_table <- renderReactable({
      girls_data <- top_items_data() |> filter(gender_clean == "girl")
      create_top_items_table(girls_data, "#C06C84")
    })
    
    output$single_table <- renderReactable({
      color <- if (input$gender_filter == "boy") "#0D677C" else if (input$gender_filter == "girl") "#C06C84" else "#4A90E2"
      
      if (input$gender_filter == "both_combined") {
        reactable(
          top_items_data() |> select(rank, clean_item_name, count, percent),
          columns = list(
            rank = colDef(name = "Rank", width = 60, style = list(fontWeight = "bold")),
            clean_item_name = colDef(name = "Item", minWidth = 150),
            count = colDef(name = "Count", width = 80, style = function(value) {
              list(fontWeight = "600", color = color)
            }),
            percent = colDef(
              name = "Percent",
              width = 100,
              format = colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                list(background = paste0(color, "20"), borderLeft = paste0("4px solid ", color))
              }
            )
          ),
          defaultPageSize = 20,
          striped = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          style = list(fontSize = "14px")
        )
      } else {
        create_top_items_table(top_items_data(), color)
      }
    })
  })
}
