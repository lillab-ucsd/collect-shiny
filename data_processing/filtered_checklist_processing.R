library(tidyverse)
library(here)

# File paths
read_path <- here("data")
write_path <- here("data")
file_name <- "collect_v1_processed_data.csv"

# Load data
d <- read_csv(here(read_path, file_name))

# Load the checklist dictionary
checklist_dict <- read_csv(here(read_path, "checklist_dictionary.csv")) |>
  rename(checklist_item = ID)


# Convert to long format
checklist_long <- d |>
  pivot_longer(
    cols = collect_checklist_1:collect_checklist_40, # all checklist columns
    names_to = "checklist_item",
    values_to = "checklist_value"
  ) |>
  left_join(checklist_dict, by = "checklist_item") |>
  select(
    participant_id = response_id,
    gender_clean,
    age_num,
    age_bin,
    clean_item_name,
    checklist_value
  ) |>
  filter(!is.na(checklist_value))  # remove rows with NA selections


# Save as CSV
write_csv(checklist_long, here(write_path, "collect_v1_filtered_checklist.csv"))