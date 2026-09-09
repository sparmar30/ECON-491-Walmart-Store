suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Read the prepared store-day dataset from the first data-preparation task.
sales <- read.csv("data/store_daily_sales_prepared.csv", check.names = FALSE) %>%
  mutate(
    date = as.Date(date),
    weekday = factor(
      weekday,
      levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    ),
    month_name = factor(
      month.name[month],
      levels = month.name
    )
  )

# Create output folders for figures and tables.
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Overall Walmart series: one row per date, summed across all stores.
overall_sales <- sales %>%
  group_by(date, day_id, day_number, weekday, month, month_name, year,
           event_name_1, event_type_1, event_name_2, event_type_2,
           snap_CA, snap_TX, snap_WI) %>%
  summarise(total_quantity = sum(total_quantity), .groups = "drop") %>%
  arrange(date)

# GOAL 1: Time-series graphs.
total_sales_plot <- ggplot(overall_sales, aes(x = date, y = total_quantity)) +
  geom_line(color = "#1f78b4", linewidth = 0.35) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Total Walmart Quantity Sold Over Time",
    x = "Date",
    y = "Quantity Sold"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = "output/figures/01_total_sales_timeseries.png",
  plot = total_sales_plot,
  width = 9,
  height = 5,
  dpi = 300
)

store_sales_plot <- ggplot(sales, aes(x = date, y = total_quantity)) +
  geom_line(color = "#2c7fb8", linewidth = 0.25) +
  facet_wrap(~ store_id, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Daily Quantity Sold by Store",
    x = "Date",
    y = "Quantity Sold"
  ) +
  theme_minimal(base_size = 10)

ggsave(
  filename = "output/figures/02_sales_timeseries_by_store.png",
  plot = store_sales_plot,
  width = 10,
  height = 8,
  dpi = 300
)

# GOAL 2: Weekly seasonality.
weekday_summary <- overall_sales %>%
  group_by(weekday) %>%
  summarise(
    mean_quantity = mean(total_quantity),
    median_quantity = median(total_quantity),
    .groups = "drop"
  ) %>%
  arrange(weekday)

store_weekday_summary <- sales %>%
  group_by(store_id, weekday) %>%
  summarise(
    mean_quantity = mean(total_quantity),
    median_quantity = median(total_quantity),
    .groups = "drop"
  ) %>%
  arrange(store_id, weekday)

weekday_boxplot <- ggplot(overall_sales, aes(x = weekday, y = total_quantity)) +
  geom_boxplot(fill = "#a6cee3", color = "#333333", outlier.alpha = 0.45) +
  labs(
    title = "Overall Walmart Quantity Sold by Weekday",
    x = "Weekday",
    y = "Quantity Sold"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = "output/figures/03_weekday_boxplot.png",
  plot = weekday_boxplot,
  width = 8,
  height = 5,
  dpi = 300
)

# GOAL 3: Monthly seasonality.
month_summary <- overall_sales %>%
  group_by(month_name) %>%
  summarise(
    mean_quantity = mean(total_quantity),
    median_quantity = median(total_quantity),
    .groups = "drop"
  ) %>%
  arrange(month_name)

month_boxplot <- ggplot(overall_sales, aes(x = month_name, y = total_quantity)) +
  geom_boxplot(fill = "#b2df8a", color = "#333333", outlier.alpha = 0.45) +
  labs(
    title = "Overall Walmart Quantity Sold by Month",
    x = "Month",
    y = "Quantity Sold"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(
  filename = "output/figures/04_month_boxplot.png",
  plot = month_boxplot,
  width = 8,
  height = 5,
  dpi = 300
)

# GOAL 4: Outlier detection using a 31-day centered rolling median baseline.
rolling_median <- function(x, window = 31) {
  half_window <- floor(window / 2)
  vapply(seq_along(x), function(i) {
    start <- max(1, i - half_window)
    end <- min(length(x), i + half_window)
    median(x[start:end], na.rm = TRUE)
  }, numeric(1))
}

overall_with_baseline <- overall_sales %>%
  mutate(
    rolling_median_31d = rolling_median(total_quantity, 31),
    deviation = total_quantity - rolling_median_31d,
    abs_deviation = abs(deviation)
  )

deviation_scale <- mad(overall_with_baseline$deviation, center = 0, constant = 1, na.rm = TRUE)

overall_with_baseline <- overall_with_baseline %>%
  mutate(
    robust_z = if (deviation_scale > 0) deviation / deviation_scale else NA_real_,
    direction = if_else(deviation >= 0, "high", "low")
  )

store_with_baseline <- sales %>%
  group_by(store_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    store_rolling_median_31d = rolling_median(total_quantity, 31),
    store_deviation = total_quantity - store_rolling_median_31d
  ) %>%
  ungroup()

candidate_dates <- overall_with_baseline %>%
  arrange(desc(abs(robust_z))) %>%
  slice_head(n = 20) %>%
  select(date, direction, total_quantity, rolling_median_31d, deviation, robust_z,
         weekday, month, year, event_name_1, event_type_1, event_name_2, event_type_2,
         snap_CA, snap_TX, snap_WI)

store_contribution_summary <- store_with_baseline %>%
  semi_join(candidate_dates, by = "date") %>%
  group_by(date) %>%
  summarise(
    stores_above_local_baseline = sum(store_deviation > 0),
    stores_below_local_baseline = sum(store_deviation < 0),
    top_contributing_stores = paste(
      store_id[order(abs(store_deviation), decreasing = TRUE)][1:3],
      collapse = ", "
    ),
    .groups = "drop"
  )

event_repeat_summary <- overall_sales %>%
  filter(!is.na(event_name_1)) %>%
  group_by(event_name_1) %>%
  summarise(
    event_years_in_data = paste(sort(unique(year)), collapse = ", "),
    event_observations = n(),
    event_median_quantity = median(total_quantity),
    .groups = "drop"
  )

same_month_day_summary <- overall_sales %>%
  mutate(month_day = format(date, "%m-%d")) %>%
  group_by(month_day) %>%
  summarise(
    same_calendar_date_years = paste(sort(unique(year)), collapse = ", "),
    same_calendar_date_median_quantity = median(total_quantity),
    .groups = "drop"
  )

outlier_candidates <- candidate_dates %>%
  mutate(month_day = format(date, "%m-%d")) %>%
  left_join(store_contribution_summary, by = "date") %>%
  left_join(event_repeat_summary, by = "event_name_1") %>%
  left_join(same_month_day_summary, by = "month_day") %>%
  mutate(
    anomaly_scope = case_when(
      direction == "high" & stores_above_local_baseline >= 7 ~ "across most stores",
      direction == "low" & stores_below_local_baseline >= 7 ~ "across most stores",
      TRUE ~ "concentrated in fewer stores"
    ),
    explanation_status = case_when(
      !is.na(event_name_1) & event_observations > 1 ~ "recurring calendar event to compare",
      !is.na(event_name_1) & event_observations == 1 ~ "one-off calendar event in this sample",
      TRUE ~ "unexplained by available calendar event fields"
    )
  ) %>%
  select(
    date, total_quantity, direction, deviation, robust_z, rolling_median_31d,
    weekday, month, year, event_name_1, event_type_1, event_name_2, event_type_2,
    snap_CA, snap_TX, snap_WI, anomaly_scope, top_contributing_stores,
    stores_above_local_baseline, stores_below_local_baseline,
    event_years_in_data, event_observations, event_median_quantity,
    same_calendar_date_years, same_calendar_date_median_quantity,
    explanation_status
  )

write.csv(outlier_candidates, "output/outlier_candidates.csv", row.names = FALSE)

# GOAL 5 and GOAL 6: Print concise findings for report drafting.
highest_weekday_median <- weekday_summary %>% slice_max(median_quantity, n = 1, with_ties = FALSE)
lowest_weekday_median <- weekday_summary %>% slice_min(median_quantity, n = 1, with_ties = FALSE)
highest_weekday_mean <- weekday_summary %>% slice_max(mean_quantity, n = 1, with_ties = FALSE)
lowest_weekday_mean <- weekday_summary %>% slice_min(mean_quantity, n = 1, with_ties = FALSE)
highest_month_median <- month_summary %>% slice_max(median_quantity, n = 1, with_ties = FALSE)
lowest_month_median <- month_summary %>% slice_min(median_quantity, n = 1, with_ties = FALSE)
highest_month_mean <- month_summary %>% slice_max(mean_quantity, n = 1, with_ties = FALSE)
lowest_month_mean <- month_summary %>% slice_min(mean_quantity, n = 1, with_ties = FALSE)

cat("EDA validation\n")
cat("--------------\n")
cat("Input observations:", nrow(sales), "\n")
cat("Stores:", paste(sort(unique(sales$store_id)), collapse = ", "), "\n")
cat("Date range:", as.character(min(sales$date)), "to", as.character(max(sales$date)), "\n")
cat("Overall daily observations:", nrow(overall_sales), "\n")
cat("Figures saved under output/figures/\n")
cat("Outlier table saved to output/outlier_candidates.csv\n\n")

cat("Weekly seasonality: overall Walmart sales\n")
print(weekday_summary)
cat("\nHighest median weekday:", as.character(highest_weekday_median$weekday), "\n")
cat("Lowest median weekday:", as.character(lowest_weekday_median$weekday), "\n")
cat("Highest mean weekday:", as.character(highest_weekday_mean$weekday), "\n")
cat("Lowest mean weekday:", as.character(lowest_weekday_mean$weekday), "\n\n")

cat("Monthly seasonality: overall Walmart sales\n")
print(month_summary)
cat("\nHighest median month:", as.character(highest_month_median$month_name), "\n")
cat("Lowest median month:", as.character(lowest_month_median$month_name), "\n")
cat("Highest mean month:", as.character(highest_month_mean$month_name), "\n")
cat("Lowest mean month:", as.character(lowest_month_mean$month_name), "\n\n")

cat("Store-level weekday medians and means\n")
print(store_weekday_summary)

cat("\nOutlier method\n")
cat("Candidate outliers are ranked by absolute deviation from a centered 31-day rolling median\n")
cat("of total Walmart quantity sold. The robust_z value divides the deviation by the MAD of\n")
cat("all daily deviations. Outliers are not deleted or changed.\n\n")

cat("Top outlier candidates\n")
print(outlier_candidates %>%
        select(date, total_quantity, direction, deviation, robust_z, weekday,
               event_name_1, event_type_1, event_name_2, event_type_2,
               snap_CA, snap_TX, snap_WI, anomaly_scope, top_contributing_stores,
               explanation_status) %>%
        slice_head(n = 20))

cat("\nModeling implications\n")
cat("- Weekday dummy variables appear useful if weekday averages/medians differ meaningfully.\n")
cat("- Month-of-year dummy variables may be useful, but month comparisons can be affected by trend.\n")
cat("- Holiday/event dummy variables should be considered for recurring calendar events.\n")
cat("- One-off outlier dummies may be appropriate for large unexplained or one-time anomalies.\n")
cat("- Store fixed effects are likely useful because stores differ in sales levels.\n")
cat("- Trend should be considered because the time-series graph may mix seasonality with long-run movement.\n")
