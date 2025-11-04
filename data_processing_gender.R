library(tidyverse)
library(here)
library(cowplot)

read_path <- here("data")
write_path <- here("data")
file_name <- "collect_v1_processed_data.csv"

d <- read_csv(here(read_path, file_name))

checklist_dict <- read_csv(here("data", "checklist_dictionary.csv")) |>
  rename(checklist_item = ID)

# Convert wide checklist columns to long format (one row per participant per item) 
# and join with dictionary to get clean item names
d_checklist_long <- d |>
  pivot_longer(
    cols = collect_checklist_1:collect_checklist_40,
    names_to = "checklist_item",
    values_to = "checklist_value"
  ) |>
  left_join(checklist_dict, by = "checklist_item")

# total participants per gender
participants_by_gender <- d_checklist_long |>
  distinct(response_id, gender_clean) |>
  count(gender_clean, name = "num_participants")

# count the selected item by gender
collect_checklist <- d_checklist_long |>
  filter(!is.na(checklist_value)) |>
  group_by(clean_item_name, gender_clean) |>
  summarize(count = n(), .groups = "drop") |>
  left_join(participants_by_gender, by = "gender_clean") |>
  mutate(percent = count / num_participants)

write_csv(collect_checklist, here(write_path, "collect_v1_checklist_count_gender.csv"))
