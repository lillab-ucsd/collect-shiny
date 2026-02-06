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
               # Number of top items
               sliderInput(ns("n_items_align"), 
                           "Number of Top Items to Show:",
                           min = 5, max = 40, value = 10, step = 1),
               
               hr(),
               
               # Age binning mode
               radioButtons(ns("age_mode"), 
                            "Age Range Mode:",
                            choices = c("All Ages" = "all",
                                        "Automatic Bins" = "auto",
                                        "Custom Range" = "manual"),
                            selected = "all"),
               
               # Automatic binning options
               conditionalPanel(
                 condition = "input.age_mode == 'auto'",
                 ns = ns,
                 sliderInput(ns("n_bins"), 
                             "Number of Age Bins:",
                             min = 2, max = 4, value = 3, step = 1)
               ),
               
               # Manual/Custom range option
               conditionalPanel(
                 condition = "input.age_mode == 'manual'",
                 ns = ns,
                 uiOutput(ns("manual_range_ui"))
               )
             ),
             
             mainPanel(
               plotOutput(ns("alignmentPlot"), height = "800px")
             )
           ))
}

alignment_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Get age range from data
    age_range <- reactive({
      c(floor(min(data$age_num, na.rm = TRUE)), 
        ceiling(max(data$age_num, na.rm = TRUE)))
    })
    
    # Dynamic UI for custom age range (single range slider)
    output$manual_range_ui <- renderUI({
      age_min <- age_range()[1]
      age_max <- age_range()[2]
      
      sliderInput(ns("custom_range"),
                  label = "Select Age Range:",
                  min = age_min,
                  max = age_max,
                  value = c(age_min, age_max),
                  step = 1,
                  sep = "")
    })
    
    # Main plot output
    output$alignmentPlot <- renderPlot({
      
      if (input$age_mode == "all") {
        # Single plot with all ages
        df_props <- compute_gender_proportions(data)
        top_items <- get_top_items_by_gender(df_props, input$n_items_align)
        
        boys_df <- top_items |> filter(gender_clean == "boy")
        girls_df <- top_items |> filter(gender_clean == "girl")
        shared_df <- join_shared_items(boys_df, girls_df)
        
        totals <- count_participants_by_gender(data)
        n_boys <- totals |> filter(gender_clean == "boy") |> pull(total)
        n_girls <- totals |> filter(gender_clean == "girl") |> pull(total)
        
        plot_alignment_ranked(boys_df, girls_df, shared_df, n_boys, n_girls)
        
      } else if (input$age_mode == "auto") {
        # Multiple plots with automatic bins
        req(input$n_bins)
        plot_alignment_age_bins(data, input$n_items_align, input$n_bins)
        
      } else if (input$age_mode == "manual") {
        # Single plot with custom age range
        req(input$custom_range)
        
        min_age <- input$custom_range[1]
        max_age <- input$custom_range[2]
        
        df_props <- compute_gender_proportions(data, min_age, max_age)
        top_items <- get_top_items_by_gender(df_props, input$n_items_align)
        
        boys_df <- top_items |> filter(gender_clean == "boy")
        girls_df <- top_items |> filter(gender_clean == "girl")
        shared_df <- join_shared_items(boys_df, girls_df)
        
        totals <- count_participants_by_gender(data, min_age, max_age)
        n_boys <- totals |> filter(gender_clean == "boy") |> pull(total)
        n_girls <- totals |> filter(gender_clean == "girl") |> pull(total)
        
        age_label <- paste0(min_age, "-", max_age)
        plot_alignment_ranked(boys_df, girls_df, shared_df, n_boys, n_girls, age_label)
      }
    })
  })
}

# alignment_ui <- function(id) {
#   ns <- NS(id)
#   
#   tabPanel("Alignment Visualization",
#            sidebarLayout(
#              sidebarPanel(
#                sliderInput(ns("n_items_align"), "Number of Top Items to Show:",
#                            min = 5, max = 40, value = 10, step = 1)
#              ),
#              mainPanel(
#                plotOutput(ns("alignmentPlot"), height = "800px")
#              )
#            ))
# }
# 
# alignment_server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#     
#     df_props <- reactive({
#       compute_gender_proportions(data)
#     })
#     
#     top_items <- reactive({
#       get_top_items_by_gender(df_props(), input$n_items_align)
#     })
#     
#     totals <- reactive({
#       count_participants_by_gender(data)
#     })
#     
#     output$alignmentPlot <- renderPlot({
#       df <- top_items()
#       boys_df <- df |> filter(gender_clean == "boy")
#       girls_df <- df |> filter(gender_clean == "girl")
#       shared_df <- join_shared_items(boys_df, girls_df)
#       
#       n_boys <- totals() |> filter(gender_clean == "boy") |> pull(total)
#       n_girls <- totals() |> filter(gender_clean == "girl") |> pull(total)
#       
#       plot_alignment_ranked(boys_df, girls_df, shared_df, n_boys, n_girls)
#     })
#   })
# }
