library(tidyverse)
library(here)
library(cowplot)

read_path <- here::here("data")
write_path <- here::here("data")
file_name <- "collect_v1_processed_data.csv"
d <- read_csv(here::here(read_path,file_name))
checklist_dict <- read.csv(here("data","checklist_dictionary.csv")) %>%
  rename(checklist_item=ID)

d_checklist_long <- d %>%
  group_by(response_id, gender_clean) %>%
  pivot_longer(
    cols=collect_checklist_1:collect_checklist_40,
    names_to="checklist_item",
    values_to="checklist_value"
  ) %>%
  select(response_id,gender_clean,checklist_item,checklist_value) %>%
  left_join(checklist_dict)
  

collect_checklist <- d_checklist_long %>%
  ungroup() %>%
  mutate(
    num_participants=n_distinct(response_id)
  ) %>%
  filter(!is.na(checklist_value)) %>%
  group_by(clean_item_name,num_participants) %>% 
  summarize(count = n()) %>%
  mutate(percent=count/num_participants)

write_csv(collect_checklist,here(write_path,"collect_v1_checklist_count.csv"))
