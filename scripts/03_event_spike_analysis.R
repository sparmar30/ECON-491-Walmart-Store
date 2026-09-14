suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# This script investigates calendar/event patterns and the largest unexplained
# high-sales spike from the prior EDA. It does not modify raw data.
sales <- read.csv("data/store_daily_sales_prepared.csv", check.names = FALSE) %>%
  mutate(
    date = as.Date(date),
    weekday = factor(
      weekday,
      levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    ),
    event_name_1 = na_if(event_name_1, ""),
    event_type_1 = na_if(event_type_1, ""),
    event_name_2 = na_if(event_name_2, ""),
    event_type_2 = na_if(event_type_2, ""),
    has_event = !is.na(event_name_1) | !is.na(event_name_2),
    event_name = case_when(
      !is.na(event_name_1) & !is.na(event_name_2) ~ paste(event_name_1, event_name_2, sep = " + "),
      !is.na(event_name_1) ~ event_name_1,
      !is.na(event_name_2) ~ event_name_2,
      TRUE ~ NA_character_
    ),
    event_type = case_when(
      !is.na(event_type_1) & !is.na(event_type_2) ~ paste(event_type_1, event_type_2, sep = " + "),
      !is.na(event_type_1) ~ event_type_1,
      !is.na(event_type_2) ~ event_type_2,
      TRUE ~ NA_character_
    )
  )

dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Event lift definition:
# For each store and weekday, estimate normal sales as the median total_quantity
# on non-event dates. Event lift is event-day quantity minus that normal same-store
# same-weekday median. Percent lift divides by that same baseline.
store_weekday_baseline <- sales %>%
  filter(!has_event) %>%
  group_by(store_id, state_id, weekday) %>%
  summarise(normal_quantity = median(total_quantity), .groups = "drop")

event_lift <- sales %>%
  filter(has_event) %>%
  left_join(store_weekday_baseline, by = c("store_id", "state_id", "weekday")) %>%
  mutate(
    lift = total_quantity - normal_quantity,
    pct_lift = lift / normal_quantity
  )

event_summary <- event_lift %>%
  group_by(event_name, event_type) %>%
  summarise(
    event_store_days = n(),
    event_dates = n_distinct(date),
    avg_lift = mean(lift, na.rm = TRUE),
    median_lift = median(lift, na.rm = TRUE),
    avg_pct_lift = mean(pct_lift, na.rm = TRUE),
    min_lift = min(lift, na.rm = TRUE),
    max_lift = max(lift, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(avg_lift)

write.csv(event_summary, "output/event_impact_summary.csv", row.names = FALSE)

event_plot_data <- event_summary %>%
  slice_max(abs(avg_lift), n = 18, with_ties = FALSE) %>%
  mutate(event_name = reorder(event_name, avg_lift))

event_impact_plot <- ggplot(event_plot_data, aes(x = event_name, y = avg_lift, fill = event_type)) +
  geom_col(color = "#333333", linewidth = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.3) +
  labs(
    title = "Average Event-Day Sales Lift Relative to Same Store-Weekday Baseline",
    x = "Calendar Event",
    y = "Average Quantity Lift",
    fill = "Event Type"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = "output/figures/05_event_impact_by_event.png",
  plot = event_impact_plot,
  width = 9,
  height = 6,
  dpi = 300
)

top_store_events <- event_summary %>%
  slice_max(abs(avg_lift), n = 8, with_ties = FALSE) %>%
  pull(event_name)

store_event_summary <- event_lift %>%
  filter(event_name %in% top_store_events) %>%
  group_by(store_id, state_id, event_name) %>%
  summarise(avg_lift = mean(lift, na.rm = TRUE), .groups = "drop")

write.csv(store_event_summary, "output/store_event_impact_summary.csv", row.names = FALSE)

store_event_plot <- ggplot(
  store_event_summary,
  aes(x = reorder(event_name, avg_lift), y = avg_lift, fill = state_id)
) +
  geom_col(color = "#333333", linewidth = 0.15) +
  coord_flip() +
  facet_wrap(~ store_id, ncol = 2) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.25) +
  labs(
    title = "Event-Day Sales Lift by Store for Largest Average Event Effects",
    x = "Calendar Event",
    y = "Average Quantity Lift",
    fill = "State"
  ) +
  theme_minimal(base_size = 9)

ggsave(
  filename = "output/figures/06_event_impact_by_store.png",
  plot = store_event_plot,
  width = 10,
  height = 9,
  dpi = 300
)

# Investigate the largest unexplained high outlier from the prior EDA.
spike_date <- as.Date("2016-03-06")
spike_day_id <- unique(sales$day_id[sales$date == spike_date])
spike_day_number <- unique(sales$day_number[sales$date == spike_date])
local_window <- 15

overall_daily <- sales %>%
  group_by(date, day_id, day_number, weekday, month, year,
           event_name_1, event_type_1, event_name_2, event_type_2,
           snap_CA, snap_TX, snap_WI) %>%
  summarise(total_quantity = sum(total_quantity), .groups = "drop") %>%
  arrange(date)

spike_nearby_overall <- overall_daily %>%
  filter(
    day_number >= spike_day_number - local_window,
    day_number <= spike_day_number + local_window,
    date != spike_date
  )

spike_overall <- overall_daily %>%
  filter(date == spike_date) %>%
  mutate(
    local_baseline_quantity = median(spike_nearby_overall$total_quantity),
    deviation_from_local_baseline = total_quantity - local_baseline_quantity,
    pct_deviation_from_local_baseline = deviation_from_local_baseline / local_baseline_quantity
  )

spike_store_baseline <- sales %>%
  filter(
    day_number >= spike_day_number - local_window,
    day_number <= spike_day_number + local_window,
    date != spike_date
  ) %>%
  group_by(store_id, state_id) %>%
  summarise(normal_store_quantity = median(total_quantity), .groups = "drop")

spike_decomposition <- sales %>%
  filter(date == spike_date) %>%
  select(store_id, state_id, date, quantity_on_spike_date = total_quantity,
         weekday, event_name_1, event_type_1, event_name_2, event_type_2,
         snap_CA, snap_TX, snap_WI, day_id, day_number) %>%
  left_join(spike_store_baseline, by = c("store_id", "state_id")) %>%
  mutate(
    difference = quantity_on_spike_date - normal_store_quantity,
    percentage_difference = difference / normal_store_quantity,
    share_of_overall_spike = difference / sum(difference)
  ) %>%
  arrange(desc(difference))

write.csv(spike_decomposition, "output/spike_decomposition_by_store.csv", row.names = FALSE)

# Reconstruct spike totals directly from raw product rows and identify top product contributors.
sales_train <- read.csv("data/sales_train_validation.csv", check.names = FALSE)
sales_test <- read.csv("data/sales_test_validation.csv", check.names = FALSE)
raw_sales <- if (spike_day_number <= 1913) sales_train else sales_test

duplicate_product_store_keys <- sum(duplicated(raw_sales[c("item_id", "dept_id", "cat_id", "store_id", "state_id")]))
spike_raw_total <- sum(raw_sales[[spike_day_id]], na.rm = TRUE)
spike_agg_store_total <- sum(sales$total_quantity[sales$date == spike_date])
spike_overall_plotted_total <- spike_overall$total_quantity

top_spike_stores <- spike_decomposition %>%
  slice_head(n = 3) %>%
  pull(store_id)

nearby_day_ids <- paste0(
  "d_",
  seq(spike_day_number - local_window, spike_day_number + local_window)
)
nearby_day_ids <- setdiff(nearby_day_ids, spike_day_id)
nearby_day_ids <- nearby_day_ids[nearby_day_ids %in% names(raw_sales)]

product_contributors <- raw_sales %>%
  filter(store_id %in% top_spike_stores) %>%
  rowwise() %>%
  mutate(
    quantity_on_spike_date = as.numeric(.data[[spike_day_id]]),
    normal_product_quantity = median(c_across(all_of(nearby_day_ids)), na.rm = TRUE),
    difference = quantity_on_spike_date - normal_product_quantity
  ) %>%
  ungroup() %>%
  mutate(
    percentage_difference = if_else(
      normal_product_quantity > 0,
      difference / normal_product_quantity,
      NA_real_
    )
  ) %>%
  group_by(store_id) %>%
  mutate(
    share_of_store_spike = difference / sum(difference, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(store_id, state_id, item_id, dept_id, cat_id,
         quantity_on_spike_date, normal_product_quantity, difference,
         percentage_difference, share_of_store_spike) %>%
  arrange(desc(difference)) %>%
  slice_head(n = 50)

write.csv(product_contributors, "output/spike_top_product_contributors.csv", row.names = FALSE)

product_breadth <- raw_sales %>%
  filter(store_id %in% top_spike_stores) %>%
  rowwise() %>%
  mutate(
    quantity_on_spike_date = as.numeric(.data[[spike_day_id]]),
    normal_product_quantity = median(c_across(all_of(nearby_day_ids)), na.rm = TRUE),
    difference = quantity_on_spike_date - normal_product_quantity
  ) %>%
  ungroup() %>%
  group_by(store_id) %>%
  summarise(
    products_checked = n(),
    products_above_baseline = sum(difference > 0, na.rm = TRUE),
    products_at_or_below_baseline = sum(difference <= 0, na.rm = TRUE),
    max_single_product_quantity = max(quantity_on_spike_date, na.rm = TRUE),
    max_single_product_difference = max(difference, na.rm = TRUE),
    top_10_product_difference_share = sum(sort(difference, decreasing = TRUE)[1:10], na.rm = TRUE) /
      sum(difference, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(product_breadth, "output/spike_product_breadth_summary.csv", row.names = FALSE)

# Calendar/event context for the spike date and nearby dates.
spike_calendar_window <- overall_daily %>%
  filter(
    day_number >= spike_day_number - 7,
    day_number <= spike_day_number + 7
  ) %>%
  select(date, total_quantity, weekday, event_name_1, event_type_1,
         event_name_2, event_type_2, snap_CA, snap_TX, snap_WI)

write.csv(spike_calendar_window, "output/spike_calendar_window.csv", row.names = FALSE)

# Structural break screen: compare windows before and after the spike, and save a rolling-average plot.
rolling_mean <- function(x, window = 28) {
  half_window <- floor(window / 2)
  vapply(seq_along(x), function(i) {
    start <- max(1, i - half_window)
    end <- min(length(x), i + half_window)
    mean(x[start:end], na.rm = TRUE)
  }, numeric(1))
}

structural_break_windows <- overall_daily %>%
  mutate(period = case_when(
    day_number >= spike_day_number - 28 & day_number <= spike_day_number - 1 ~ "28 days before",
    day_number >= spike_day_number + 1 & day_number <= spike_day_number + 28 ~ "28 days after",
    day_number >= spike_day_number - 56 & day_number <= spike_day_number - 29 ~ "29-56 days before",
    day_number >= spike_day_number + 29 & day_number <= spike_day_number + 56 ~ "29-56 days after",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    days = n(),
    mean_quantity = mean(total_quantity),
    median_quantity = median(total_quantity),
    .groups = "drop"
  )

write.csv(structural_break_windows, "output/spike_structural_break_window_summary.csv", row.names = FALSE)

rolling_plot_data <- overall_daily %>%
  mutate(rolling_mean_28d = rolling_mean(total_quantity, 28))

rolling_plot <- ggplot(rolling_plot_data, aes(x = date, y = rolling_mean_28d)) +
  geom_line(color = "#1f78b4", linewidth = 0.45) +
  geom_vline(xintercept = spike_date, linetype = "dashed", color = "#b2182b") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "28-Day Rolling Average of Total Walmart Quantity Sold",
    x = "Date",
    y = "28-Day Rolling Average Quantity"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = "output/figures/07_spike_structural_break_screen.png",
  plot = rolling_plot,
  width = 9,
  height = 5,
  dpi = 300
)

cat("Event lift definition\n")
cat("---------------------\n")
cat("Normal quantity is the median non-event sales quantity for the same store and weekday.\n")
cat("Event lift = event-day store quantity - normal same-store/same-weekday quantity.\n")
cat("Percent lift = event lift / normal quantity.\n\n")

cat("Event impact summary: strongest negative and positive average lifts\n")
print(bind_rows(
  event_summary %>% slice_head(n = 8),
  event_summary %>% slice_tail(n = 8)
) %>% arrange(avg_lift))

cat("\nSpike date summary\n")
print(spike_overall %>%
        select(date, day_id, total_quantity, local_baseline_quantity,
               deviation_from_local_baseline, pct_deviation_from_local_baseline,
               weekday, event_name_1, event_type_1, event_name_2, event_type_2,
               snap_CA, snap_TX, snap_WI))

cat("\nSpike decomposition by store\n")
print(spike_decomposition %>%
        select(store_id, state_id, quantity_on_spike_date, normal_store_quantity,
               difference, percentage_difference, share_of_overall_spike))

cat("\nProduct-level breadth for top spike stores\n")
print(product_breadth)

cat("\nTop product contributors to the spike\n")
print(product_contributors %>% slice_head(n = 20))

cat("\nRaw data verification\n")
print(list(
  spike_date = spike_date,
  spike_day_id = spike_day_id,
  duplicate_product_store_keys_in_raw_source = duplicate_product_store_keys,
  missing_spike_quantities = sum(is.na(raw_sales[[spike_day_id]])),
  negative_spike_quantities = sum(raw_sales[[spike_day_id]] < 0, na.rm = TRUE),
  max_single_product_quantity_on_spike_date = max(raw_sales[[spike_day_id]], na.rm = TRUE),
  raw_product_level_sum = spike_raw_total,
  aggregated_store_level_sum = spike_agg_store_total,
  overall_plotted_value = spike_overall_plotted_total,
  all_three_reconcile = spike_raw_total == spike_agg_store_total &&
    spike_agg_store_total == spike_overall_plotted_total
))

cat("\nCalendar/event context around spike date\n")
print(spike_calendar_window)

cat("\nStructural break screen\n")
print(structural_break_windows)
cat("The dashed line in output/figures/07_spike_structural_break_screen.png marks the spike date.\n")

cat("\nSaved outputs\n")
cat("output/figures/05_event_impact_by_event.png\n")
cat("output/figures/06_event_impact_by_store.png\n")
cat("output/figures/07_spike_structural_break_screen.png\n")
cat("output/event_impact_summary.csv\n")
cat("output/store_event_impact_summary.csv\n")
cat("output/spike_decomposition_by_store.csv\n")
cat("output/spike_top_product_contributors.csv\n")
cat("output/spike_product_breadth_summary.csv\n")
cat("output/spike_calendar_window.csv\n")
cat("output/spike_structural_break_window_summary.csv\n")
