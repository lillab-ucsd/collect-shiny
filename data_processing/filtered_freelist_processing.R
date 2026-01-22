library(tidyverse)
library(here)
library(stringdist)

# File paths
read_path <- here("data")
write_path <- here("data")

# Load data with proper encoding
processed_data <- read_csv(
  here(read_path, "collect_v1_processed_data.csv"),
)

# Load the freelist dictionary
freelist_dict <- read_csv(
  here(read_path, "collect_v1_dictionary.csv"),
)

# Clean up any encoding issues
freelist_dict <- freelist_dict |>
  mutate(
    freelist_item = iconv(freelist_item, from = "UTF-8", to = "UTF-8", sub = "")
  )

# Fix column naming inconsistency (freelist_7 should be freelist_6)
if ("collect_freelist_7" %in% names(processed_data)) {
  processed_data <- processed_data |>
    rename(collect_freelist_6 = collect_freelist_7)
}

# Clean up encoding issues in processed data freelist columns
for (col in names(processed_data)[str_detect(names(processed_data), "^collect_freelist_")]) {
  processed_data[[col]] <- iconv(processed_data[[col]], from = "UTF-8", to = "UTF-8", sub = "")
}

# ============================================================================
# Reshape data to long format
# ============================================================================

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

cat("\nMatching collections with dictionary...\n")

# Join with dictionary to get all metadata
freelist_long <- freelist_long |>
  left_join(
    freelist_dict,
    by = c("collect_freelist" = "freelist_item")
  ) |>
  rename(
    collection_name_original = collect_freelist
  ) |>
  mutate(
    is_favorite = collection_name_original == favorite,
    interest_learning = as.numeric(interest_learning),
    interest_curious = as.numeric(interest_curious),
    interest_talking = as.numeric(interest_talking),
    interest_playing = as.numeric(interest_playing)
  )

# ============================================================================
# Report matching statistics
# ============================================================================

cat("\n=== Matching Statistics ===\n")
cat("Total collections:", nrow(freelist_long), "\n")
cat("Successfully matched:", sum(!is.na(freelist_long$cleaned_name)), "\n")
cat("Unmatched:", sum(is.na(freelist_long$cleaned_name)), "\n")

# Show unmatched items if any
unmatched <- freelist_long |>
  filter(is.na(cleaned_name)) |>
  count(collection_name_original) |>
  arrange(desc(n))

if (nrow(unmatched) > 0) {
  cat("\n=== WARNING: Unmatched Items ===\n")
  print(unmatched)
  cat("\nThese items are in the data but not in the dictionary!\n")
}

# Show category distribution
cat("\n=== Category Distribution ===\n")
category_stats <- freelist_long |>
  filter(!is.na(category)) |>
  count(category) |>
  arrange(desc(n))

print(category_stats)

# Show object type distribution
cat("\n=== Object Type Distribution ===\n")
type_stats <- freelist_long |>
  filter(!is.na(object_type)) |>
  count(object_type) |>
  arrange(desc(n))

print(type_stats)

# ============================================================================
# Save processed data
# ============================================================================

write_csv(freelist_long, here(write_path, "collect_v1_freelist_long.csv"))

cat("\n✓ Saved to:", here(write_path, "collect_v1_freelist_long.csv"), "\n")

# ============================================================================
# Summary of available columns
# ============================================================================

cat("\n=== Available Columns in Output ===\n")
cat("Demographics:", paste(c("response_id", "participant_id", "age_num", "age_bin", "gender_clean"), collapse = ", "), "\n")
cat("Collection Info:", paste(c("collection_name_original", "cleaned_name", "unified_name", "collection_number", "is_favorite"), collapse = ", "), "\n")
cat("Dictionary Fields:", paste(c("object_type", "category", "natural", "taxonomic_content", "taxonomic_object"), collapse = ", "), "\n")
cat("Flags:", paste(c("animal-related", "fiction-related", "collectible"), collapse = ", "), "\n")
cat("Interest Ratings:", paste(c("interest_learning", "interest_curious", "interest_talking", "interest_playing"), collapse = ", "), "\n")
cat("Context:", paste(c("collection_origin", "activities", "collect_friends", "collection_reason", "ask_child"), collapse = ", "), "\n")