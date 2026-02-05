library(shiny)
library(tidyverse)
library(here)
library(ggforce)
library(ggrepel)
library(reactable)
library(patchwork)

# Source helper and module files
source("R/helpers.R")
source("R/top_items_proportion.R")
source("R/top_items_table.R")
source("R/age_comparison.R")
source("R/alignment.R")
source("R/interest.R")

checklist_data <- read_csv(here("data/collect_v1_checklist_long.csv"))
freelist_data <- read_csv(here("data/collect_v1_freelist_long.csv"))

ui <- navbarPage(
  title = "Collect Data Explorer",
  
  top_items_proportion_ui("top_items_proportion"),
  top_items_table_ui("top_items_table"),
  age_comparison_ui("age_compare"),
  alignment_ui("alignment"),
  interest_ui("interest")
  
)

server <- function(input, output, session) {
  
  top_items_proportion_server("top_items_proportion", checklist_data)
  top_items_table_server("top_items_table", checklist_data)
  age_comparison_server("age_compare", checklist_data)
  alignment_server("alignment", checklist_data)
  interest_server("interest", freelist_data)
  
}

shinyApp(ui, server)

