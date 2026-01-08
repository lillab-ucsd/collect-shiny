library(tidyverse)
library(here)
library(stringdist)

# File paths
read_path <- here("data")
write_path <- here("data")

# Load data with proper encoding
processed_data <- read_csv(
  here(read_path, "collect_v1_processed_data.csv"),
  locale = locale(encoding = "UTF-8")
)

freelist_items <- read_csv(
  here(read_path, "collect_v1_freelist_items.csv"),
  locale = locale(encoding = "UTF-8")
)

# Clean up any encoding issues
freelist_items <- freelist_items |>
  mutate(freelist_item = iconv(freelist_item, from = "UTF-8", to = "UTF-8", sub = ""))

# Fix column naming inconsistency (freelist_7 should be freelist_6)
if ("collect_freelist_7" %in% names(processed_data)) {
  processed_data <- processed_data |>
    rename(collect_freelist_6 = collect_freelist_7)
}

# Clean up encoding issues in processed data freelist columns
for (col in names(processed_data)[str_detect(names(processed_data), "^collect_freelist_")]) {
  processed_data[[col]] <- iconv(processed_data[[col]], from = "UTF-8", to = "UTF-8", sub = "")
}

# Function to find best matching freelist item using fuzzy matching
match_freelist_item <- function(collection_name, freelist_df, threshold = 0.2) {
  if (is.na(collection_name) || collection_name == "NA" || collection_name == "") {
    return(list(freelist_item = NA, index = NA, match_score = NA))
  }
  
  distances <- stringdist::stringdist(
    tolower(trimws(collection_name)), 
    tolower(trimws(freelist_df$freelist_item)),
    method = "jw"
  )
  
  best_match_idx <- which.min(distances)
  best_distance <- distances[best_match_idx]
  
  if (best_distance <= threshold) {
    return(list(
      freelist_item = freelist_df$freelist_item[best_match_idx],
      index = freelist_df$index[best_match_idx],
      match_score = 1 - best_distance
    ))
  } else {
    return(list(
      freelist_item = collection_name,
      index = NA,
      match_score = 1 - best_distance
    ))
  }
}

# Reshape data to long format with freelist matching
cat("Processing freelist data...\n")

collection_data <- list()

for (i in 1:6) {
  
  cat("Processing collection", i, "...\n")
  
  temp_df <- processed_data |>
    select(
      response_id, participant_id, multi_rid,
      age, age_num, age_bin, gender, gender_clean,
      l1, l1percent, other_languages,
      starts_with("race_"),
      parent_education, country, household_income,
      collect_freelist = paste0("collect_freelist_", i),
      favorite,
      collection_origin = paste0("collection_origin_", i),
      activities = paste0("activities_", i),
      collect_friends = paste0("collect_friends_", i),
      interest_learning = paste0("interest_questions_", i, "_1"),
      interest_curious = paste0("interest_questions_", i, "_2"),
      interest_talking = paste0("interest_questions_", i, "_3"),
      interest_playing = paste0("interest_questions_", i, "_4"),
      collection_reason = paste0("collection_reason_", i),
      ask_child = paste0("ask_child_", i)
    ) |>
    filter(!is.na(collect_freelist) & 
             collect_freelist != "NA" & 
             collect_freelist != "") |>
    mutate(collection_number = i)
  
  collection_data[[i]] <- temp_df
}

# Combine all collections
freelist_long <- bind_rows(collection_data)

cat("\nMatching collections with freelist items...\n")

# Match with freelist items
matches <- map_dfr(freelist_long$collect_freelist, function(x) {
  match_freelist_item(x, freelist_items)
})

# Add matched data
freelist_long <- freelist_long |>
  bind_cols(matches) |>
  rename(
    collection_name_original = collect_freelist,
    collection_name_matched = freelist_item,
    freelist_index = index,
    match_confidence = match_score
  ) |>
  mutate(
    is_favorite = collection_name_original == favorite,
    interest_learning = as.numeric(interest_learning),
    interest_curious = as.numeric(interest_curious),
    interest_talking = as.numeric(interest_talking),
    interest_playing = as.numeric(interest_playing)
  )

# Report matching statistics
cat("\n=== Matching Statistics ===\n")
cat("Total collections:", nrow(freelist_long), "\n")
cat("Matched to freelist:", sum(!is.na(freelist_long$freelist_index)), "\n")
cat("Unmatched:", sum(is.na(freelist_long$freelist_index)), "\n")
cat("Average match confidence:", round(mean(freelist_long$match_confidence, na.rm = TRUE), 3), "\n")

# Show unmatched items
unmatched <- freelist_long |>
  filter(is.na(freelist_index)) |>
  count(collection_name_original) |>
  arrange(desc(n))

if (nrow(unmatched) > 0) {
  cat("\n=== Unmatched Items (top 10) ===\n")
  print(head(unmatched, 10))
}

# Save as CSV
write_csv(freelist_long, here(write_path, "collect_v1_filtered_freelist.csv"))

cat("\n✓ Saved to:", here(write_path, "collect_v1_freelist_long.csv"), "\n")
