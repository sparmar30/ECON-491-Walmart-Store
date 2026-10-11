# Run from the repository root: Rscript scripts/04_ols_baseline.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
if (!requireNamespace("stargazer", quietly = TRUE)) {
  stop("Install stargazer first: install.packages('stargazer')")
}
out <- "output/ols_baseline"
dir.create(file.path(out, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, "tables"), recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
unique_key <- function(x, keys) check(!anyDuplicated(x[keys]), paste("Duplicate key:", paste(keys, collapse = ", ")))

# Validate the raw schemas and consecutive day ranges before combining sales.
train_raw <- read.csv("data/sales_train_validation.csv", check.names = FALSE)
test_raw <- read.csv("data/sales_test_validation.csv", check.names = FALSE)
calendar <- read.csv("data/calendar.csv", check.names = FALSE)
prices <- read.csv("data/sell_prices.csv", check.names = FALSE)
prepared <- read.csv("data/store_daily_sales_prepared.csv", check.names = FALSE)
prepared$date <- as.Date(prepared$date)
ids <- c("item_id", "dept_id", "cat_id", "store_id", "state_id")
train_days <- grep("^d_[0-9]+$", names(train_raw), value = TRUE)
test_days <- grep("^d_[0-9]+$", names(test_raw), value = TRUE)
check(identical(train_days, paste0("d_", 1:1913)), "Unexpected training day range")
check(identical(test_days, paste0("d_", 1914:1941)), "Unexpected test day range")
unique_key(train_raw, c("store_id", "item_id"))
unique_key(test_raw, c("store_id", "item_id"))
check(nrow(anti_join(train_raw[ids], test_raw[ids], by = ids)) == 0 &&
      nrow(anti_join(test_raw[ids], train_raw[ids], by = ids)) == 0, "Sales identifiers differ")
calendar$date <- as.Date(calendar$date)
unique_key(calendar, "date")
check(!anyNA(calendar$date) && all(diff(calendar$date) == 1) &&
      min(calendar$date) == as.Date("2011-01-29"), "Calendar order is not the expected daily sequence")
calendar$day_id <- paste0("d_", seq_len(nrow(calendar)))
unique_key(prices, c("store_id", "item_id", "wm_yr_wk"))
check(!anyNA(prices[c("store_id", "item_id", "wm_yr_wk")]), "Missing price key")
unique_key(prepared, c("store_id", "date"))
stores <- sort(unique(train_raw$store_id))
check(length(stores) == 10, "Expected ten stores")

# Work one store at a time; calendar week identifies the appropriate item price.
daily_parts <- vector("list", length(stores))
audit_parts <- vector("list", length(stores))
for (j in seq_along(stores)) {
  s <- stores[j]
  message("Preparing weighted price: ", s)
  wide <- inner_join(filter(train_raw, store_id == s),
                     filter(test_raw, store_id == s), by = ids)
  long <- wide %>%
    pivot_longer(all_of(c(train_days, test_days)), names_to = "day_id", values_to = "quantity") %>%
    select(store_id, item_id, day_id, quantity)
  check(all(is.finite(long$quantity)) && all(long$quantity >= 0), "Invalid raw quantity")
  expected_rows <- nrow(long)
  long <- long %>%
    left_join(select(calendar, day_id, date, wm_yr_wk), by = "day_id") %>%
    left_join(filter(prices, store_id == s), by = c("store_id", "item_id", "wm_yr_wk"))
  check(nrow(long) == expected_rows && !anyNA(long$date), "Join multiplied or lost sales observations")
  invalid <- !is.finite(long$sell_price) | long$sell_price <= 0
  check(!any(long$quantity > 0 & invalid), paste("Positive sales lack valid prices:", s))
  audit_parts[[j]] <- data.frame(store_id = s, product_day_rows = nrow(long),
    positive_sales_missing_price = sum(long$quantity > 0 & invalid),
    zero_sales_missing_price = sum(long$quantity == 0 & invalid))
  # Zero-sales products contribute zero, even when no price is listed.
  long$expenditure <- ifelse(long$quantity > 0, long$quantity * long$sell_price, 0)
  daily_parts[[j]] <- long %>% group_by(store_id, date) %>%
    summarise(recomputed_quantity = sum(quantity), expenditure = sum(expenditure),
      min_contributing_price = if (any(quantity > 0)) min(sell_price[quantity > 0]) else NA_real_,
      max_contributing_price = if (any(quantity > 0)) max(sell_price[quantity > 0]) else NA_real_,
      .groups = "drop")
  rm(wide, long)
  invisible(gc())
}
daily <- bind_rows(daily_parts)
check(nrow(daily) == 19410 && nrow(prepared) == 19410, "Unexpected daily row count")
check(nrow(anti_join(daily, prepared, by = c("store_id", "date"))) == 0,
      "Recomputed dates differ from prepared dates")
model_data <- prepared %>% left_join(daily, by = c("store_id", "date")) %>%
  arrange(store_id, date)
check(all(model_data$total_quantity == model_data$recomputed_quantity), "Quantity reconciliation failed")

# Same-day weights are retrospective: test quantities enter this predictor.
model_data <- model_data %>%
  mutate(weighted_price_raw = ifelse(total_quantity > 0, expenditure / total_quantity, NA_real_),
         price_imputed = total_quantity == 0, weighted_price = weighted_price_raw) %>%
  group_by(store_id) %>% fill(weighted_price, .direction = "down") %>% ungroup()
check(sum(model_data$price_imputed) == 24, "Expected 24 zero-sales price imputations")
check(all(is.finite(model_data$weighted_price) & model_data$weighted_price > 0), "Unresolved weighted price")
observed <- !model_data$price_imputed
check(all(model_data$weighted_price[observed] >= model_data$min_contributing_price[observed] - 1e-10 &
          model_data$weighted_price[observed] <= model_data$max_contributing_price[observed] + 1e-10),
      "Weighted price outside contributing product price range")
for (s in stores) {
  z <- filter(model_data, store_id == s)
  check(all(diff(z$date) == 1) && nrow(z) == 1941, "Incomplete store date coverage")
  k <- which(z$price_imputed)
  check(!1L %in% k && all(z$weighted_price[k] == z$weighted_price[k - 1L]), "Carry-forward mismatch")
}

# Split by calendar date and define every predictor from training information.
cutoff <- max(model_data$date) - 27
check(cutoff == as.Date("2016-04-25"), "Unexpected holdout start")
model_data$sample <- ifelse(model_data$date < cutoff, "train", "test")
weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
model_data$weekday_factor <- factor(model_data$weekday, levels = weekday_levels)
model_data$month_factor <- factor(model_data$month, levels = 1:12, labels = month.name)
events_from <- function(x) sort(unique(na.omit(c(x$event_name_1, x$event_name_2))))
events <- events_from(filter(model_data, sample == "train"))
check(length(setdiff(events_from(filter(model_data, sample == "test")), events)) == 0,
      "Test events not represented in training")
event_map <- data.frame(event = events, term = paste0("event_", make.names(events, unique = TRUE)))
for (i in seq_along(events)) {
  model_data[[event_map$term[i]]] <- as.integer(
    model_data$event_name_1 %in% events[i] | model_data$event_name_2 %in% events[i])
}
check(!anyNA(model_data[c("weekday_factor", "month_factor")]), "Missing calendar predictor")
formula_ols <- reformulate(c("weighted_price", "weekday_factor", "month_factor", event_map$term),
                           response = "total_quantity")
models <- setNames(vector("list", length(stores)), stores)
predictions <- coefficients <- fit_stats <- vector("list", length(stores))
for (j in seq_along(stores)) {
  s <- stores[j]
  train <- filter(model_data, store_id == s, sample == "train")
  test <- filter(model_data, store_id == s, sample == "test")
  check(nrow(train) == 1913 && nrow(test) == 28, "Incorrect train/test counts")
  fit <- lm(formula_ols, data = train, na.action = na.fail)
  check(fit$rank == ncol(model.matrix(fit)) && !anyNA(coef(fit)), paste("Rank deficient model:", s))
  predicted <- as.numeric(predict(fit, newdata = test))
  check(length(predicted) == 28 && all(is.finite(predicted)), "Invalid predictions")
  models[[s]] <- fit
  predictions[[j]] <- data.frame(store_id = s, date = test$date, actual = test$total_quantity,
    predicted = predicted, error = test$total_quantity - predicted,
    squared_error = (test$total_quantity - predicted)^2, negative_prediction = predicted < 0)
  sm <- summary(fit)
  cm <- as.data.frame(sm$coefficients)
  names(cm) <- c("estimate", "std_error", "t_statistic", "p_value")
  coefficients[[j]] <- data.frame(store_id = s, term = rownames(cm), cm, row.names = NULL)
  fit_stats[[j]] <- data.frame(store_id = s, n_train = nobs(fit), rank = fit$rank,
    r_squared = sm$r.squared, adjusted_r_squared = sm$adj.r.squared,
    residual_std_error = sm$sigma, residual_df = df.residual(fit))
}
predictions <- bind_rows(predictions)
coefficients <- bind_rows(coefficients)
fit_stats <- bind_rows(fit_stats)
rmse <- predictions %>% group_by(store_id) %>%
  summarise(n_test = n(), RMSE = sqrt(mean(squared_error)), .groups = "drop") %>%
  bind_rows(data.frame(store_id = "Overall", n_test = nrow(predictions),
                      RMSE = sqrt(mean(predictions$squared_error))))
validation <- data.frame(check = c("Stores", "States", "Dates per store", "Total rows", "Training rows",
  "Test rows", "Duplicate store-date keys", "Quantity reconciliation mismatches", "Zero-sales price imputations",
  "Missing weighted prices", "Positive-sales rows missing valid prices", "Full-rank models", "Finite predictions",
  "Negative predictions"), value = c(length(stores), n_distinct(model_data$state_id), 1941,
  nrow(model_data), sum(model_data$sample == "train"), sum(model_data$sample == "test"),
  sum(duplicated(model_data[c("store_id", "date")])),
  sum(model_data$total_quantity != model_data$recomputed_quantity), sum(model_data$price_imputed),
  sum(!is.finite(model_data$weighted_price)), 0, length(models), sum(is.finite(predictions$predicted)),
  sum(predictions$negative_prediction)))
save_csv(model_data, "modeling_dataset.csv")
save_csv(predictions, "predictions.csv")
save_csv(coefficients, "coefficients.csv")
save_csv(fit_stats, "model_fit.csv")
save_csv(rmse, "rmse.csv")
save_csv(validation, "validation.csv")
save_csv(bind_rows(audit_parts), "price_coverage.csv")
save_csv(event_map, "event_dictionary.csv")
save_csv(filter(model_data, price_imputed), "price_imputations.csv")
saveRDS(models, file.path(out, "models.rds"))

# Stargazer tables show all inferential statistics; CSV preserves full precision.
write_stargazer <- function(selected, filename) {
  # Passing the short list expression avoids stargazer deparsing full lm objects.
  invisible(capture.output(stargazer::stargazer(selected, type = "html", out = file.path(out, "tables", filename),
    title = paste("Store OLS baseline:", paste(names(selected), collapse = ", ")), dep.var.labels = "Daily units sold",
    model.numbers = TRUE, digits = 3, report = "vcstp", star.cutoffs = NA)))
}
write_stargazer(models[1:5], "coefficients_group_1.html")
write_stargazer(models[6:10], "coefficients_group_2.html")
for (s in stores) write_stargazer(models[s], paste0("coefficients_", s, ".html"))

# Keep the 28 actual and predicted observations aligned on the same date axis.
plot_data <- predictions %>% select(store_id, date, Actual = actual, Predicted = predicted) %>%
  pivot_longer(c(Actual, Predicted), names_to = "series", values_to = "quantity")
plot_base <- function(d) ggplot(d, aes(date, quantity, color = series, linetype = series)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c(Actual = "#0071ce", Predicted = "#c45c14")) +
  scale_linetype_manual(values = c(Actual = "solid", Predicted = "dashed")) +
  scale_x_date(breaks = seq(cutoff, max(model_data$date), by = "7 days"), date_labels = "%b %d") +
  labs(x = NULL, y = "Units sold", color = NULL, linetype = NULL,
       caption = "Retrospective predictions: weighted price uses actual same-day product quantities.") +
  theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.minor = element_blank())
ggsave(file.path(out, "figures", "forecast_all_stores.png"),
  plot_base(plot_data) + facet_wrap(~store_id, ncol = 2, scales = "free_y") +
    labs(title = "OLS baseline: actual and predicted quantities", subtitle = "April 25-May 22, 2016 | separate model per store"),
  width = 13, height = 16, dpi = 160)
for (s in stores) ggsave(file.path(out, "figures", paste0("forecast_", s, ".png")),
  plot_base(filter(plot_data, store_id == s)) + labs(title = paste(s, "- actual and predicted quantities")),
  width = 10, height = 5, dpi = 160)

# Recalculate the metric from the saved predictions rather than model objects.
saved <- read.csv(file.path(out, "predictions.csv"))
recalculated <- sqrt(mean((saved$actual - saved$predicted)^2))
check(isTRUE(all.equal(recalculated, rmse$RMSE[rmse$store_id == "Overall"], tolerance = 1e-10)),
      "Saved overall RMSE does not reconcile")
for (s in stores) check(abs(sqrt(mean(with(subset(saved, store_id == s), (actual - predicted)^2))) -
  rmse$RMSE[rmse$store_id == s]) < 1e-8, "Saved store RMSE does not reconcile")
writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
cat("\nValidation\n"); print(validation, row.names = FALSE)
cat("\nRMSE in units sold\n"); print(rmse)
