# ============================================================================
# FILE: eda_freelist.R
# PURPOSE: Exploratory Data Analysis and Quality Checks for Freelist Data
# DESCRIPTION: Verify data quality, check sample sizes, and validate joins
# ============================================================================

library(tidyverse)
library(here)

# Load data
df_freelist <- read_csv(here("data/collect_v1_freelist_long.csv"))
df_original <- read_csv(here("data/collect_v1_processed_data.csv"))
df_dict <- read_csv(here("data/collect_v1_dictionary.csv"))

df_freelist |>
  count(cleaned_name, sort=TRUE)