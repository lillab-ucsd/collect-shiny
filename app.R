library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(cowplot)

# Create a sample dataset 1
checklist_data <- read_csv("data/collect_v1_checklist_count.csv")

# Create a sample dataset 2
data2 <- data.frame(clean_item_name = c("item1", "item2", "item3", "item4", "item5"), percent = c(5, 15, 20, 25, 35))

# Define the user interface
ui <- fluidPage(
  titlePanel("Collecting Frequency"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", choices = c("2022", "1900"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Get the selected dataset
  selected_data <- reactive({
    switch(input$dataset,
           "2022" = checklist_data,
           "1900" = data2)
  })
  
  # Plot the item frequency
  output$plot <- renderPlot({
    ggplot(selected_data(),aes(reorder(clean_item_name,-percent), y = percent))+
      geom_bar(stat="identity")+
      theme_cowplot()+
      theme(axis.text.x  = element_text(angle=90, vjust=0.5,size=10))+
      ylab("Proportion Children Collecting")+
      xlab("Item")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
