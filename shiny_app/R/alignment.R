# ============================================================================
# FILE: R/alignment.R
# PURPOSE: Alignment visualization comparing boys' vs girls' top items
# DESCRIPTION: Slope graph showing top collected items for each gender with 
#              connecting lines for shared items, highlighting gender differences
# ============================================================================

alignment_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Alignment Visualization",
           sidebarLayout(
             sidebarPanel(
               sliderInput(ns("n_items_align"), "Number of Top Items to Show:",
                           min = 5, max = 40, value = 10, step = 1)
             ),
             mainPanel(
               fluidRow(
                 column(6, textOutput(ns("n_boys"))),
                 column(6, textOutput(ns("n_girls")))
               ),
               plotOutput(ns("alignmentPlot"), height = "800px")
             )
           ))
}

alignment_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    df_props <- reactive({
      compute_gender_proportions(data)
    })
    
    top_items <- reactive({
      get_top_items_by_gender(df_props(), input$n_items_align)
    })
    
    output$alignmentPlot <- renderPlot({
      df <- top_items()
      boys_df <- df |> filter(gender_clean == "boy")
      girls_df <- df |> filter(gender_clean == "girl")
      shared_df <- join_shared_items(boys_df, girls_df)
      
      plot_alignment(boys_df, girls_df, shared_df)
    })
    
    totals <- reactive({
      count_participants_by_gender(data)
    })
    
    output$n_boys <- renderText({
      paste0("n (Boys) = ",
             totals() |> filter(gender_clean == "boy") |> pull(total))
    })
    
    output$n_girls <- renderText({
      paste0("n (Girls) = ",
             totals() |> filter(gender_clean == "girl") |> pull(total))
    })
  })
}
