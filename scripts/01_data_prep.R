suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Read the three required source files. Paths are relative to the repository root.
sales_train <- read.csv("data/sales_train_validation.csv", check.names = FALSE)
sales_test <- read.csv("data/sales_test_validation.csv", check.names = FALSE)
calendar <- read.csv("data/calendar.csv", check.names = FALSE)

# Convert the sales files from wide daily columns to long product-store-day rows,
# then aggregate quantities across all products before joining calendar data.
aggregate_sales_by_store_day <- function(sales_data) {
  sales_data %>%
    select(store_id, state_id, starts_with("d_")) %>%
    pivot_longer(
      cols = starts_with("d_"),
      names_to = "day_id",
      values_to = "quantity"
    ) %>%
    mutate(
      quantity = as.numeric(quantity),
      day_number = as.integer(sub("^d_", "", day_id))
    ) %>%
    group_by(store_id, state_id, day_id, day_number) %>%
    summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop")
}

sales_train_agg <- aggregate_sales_by_store_day(sales_train)
sales_test_agg <- aggregate_sales_by_store_day(sales_test)

# Combine train and test into one continuous store-level sales history.
complete_sales_history <- bind_rows(sales_train_agg, sales_test_agg) %>%
  arrange(store_id, day_number)

# This calendar file has dates in order but no explicit d_* key, so construct it.
calendar_with_day <- calendar %>%
  mutate(
    day_number = row_number(),
    day_id = paste0("d_", day_number),
    date = as.Date(date)
  )

# Join calendar only after aggregating sales to one row per store and day.
final_store_daily_sales <- complete_sales_history %>%
  left_join(calendar_with_day, by = c("day_id", "day_number")) %>%
  select(store_id, date, total_quantity, state_id, day_id, day_number, everything()) %>%
  arrange(store_id, date)

# Save the prepared dataset as a new file. This does not change the raw CSVs.
write.csv(final_store_daily_sales, "data/store_daily_sales_prepared.csv", row.names = FALSE)

# Validation checks for the prepared time-series dataset.
unique_stores <- sort(unique(final_store_daily_sales$store_id))
unique_states <- sort(unique(final_store_daily_sales$state_id))
observations_per_store <- final_store_daily_sales %>%
  count(store_id, name = "observations")
duplicate_store_dates <- final_store_daily_sales %>%
  count(store_id, date, name = "n") %>%
  filter(n > 1) %>%
  nrow()
expected_dates <- n_distinct(final_store_daily_sales$date)
store_date_check <- observations_per_store %>%
  mutate(
    expected_dates = expected_dates,
    has_expected_number_of_dates = observations == expected_dates
  )

cat("Validation results\n")
cat("------------------\n")
cat("Number of unique stores:", length(unique_stores), "\n")
cat("Store IDs:", paste(unique_stores, collapse = ", "), "\n")
cat("Number of unique states:", length(unique_states), "\n")
cat("State IDs:", paste(unique_states, collapse = ", "), "\n")
cat("Number of unique days:", n_distinct(final_store_daily_sales$day_id), "\n")
cat("Earliest date:", as.character(min(final_store_daily_sales$date, na.rm = TRUE)), "\n")
cat("Latest date:", as.character(max(final_store_daily_sales$date, na.rm = TRUE)), "\n")
cat("Total number of final observations:", nrow(final_store_daily_sales), "\n")
cat("Number of duplicate store/date combinations:", duplicate_store_dates, "\n")
cat("Number of missing dates after calendar join:", sum(is.na(final_store_daily_sales$date)), "\n")
cat("Number of missing quantities:", sum(is.na(final_store_daily_sales$total_quantity)), "\n")
cat("Each store has the expected number of dates:", all(store_date_check$has_expected_number_of_dates), "\n\n")

cat("Observations per store\n")
print(observations_per_store)

cat("\nStore date-count check\n")
print(store_date_check)

cat("\nFirst 10 rows of final dataset\n")
print(head(final_store_daily_sales, 10))
