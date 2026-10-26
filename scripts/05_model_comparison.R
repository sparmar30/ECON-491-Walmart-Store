# Run from the repository root: Rscript scripts/05_model_comparison.R
# Dependency setup: install.packages(c('dplyr', 'tidyr', 'ggplot2', 'lightgbm'))
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})
stopifnot(requireNamespace('lightgbm', quietly = TRUE))
out <- 'output/model_comparison'
for (sub in c('', 'figures', 'models')) dir.create(file.path(out, sub), recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) write.csv(x, file.path(out, paste0(name, '.csv')), row.names = FALSE)
check <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
set.seed(491)
d <- read.csv('output/ols_baseline/modeling_dataset.csv', check.names = FALSE)
d$date <- as.Date(d$date)
d <- arrange(d, store_id, date)
original <- d
stores <- sort(unique(d$store_id))
check(nrow(d) == 19410 && length(stores) == 10 && !anyDuplicated(d[c('store_id', 'date')]), 'Invalid store-date keys')
check(all(is.finite(d$weighted_price)) && sum(d$price_imputed) == 24, 'Invalid saved prices')
calendar <- read.csv('data/calendar.csv')
calendar$date <- as.Date(calendar$date)
check(!anyDuplicated(calendar$date) && all(diff(calendar$date) == 1), 'Invalid calendar')
d$weekday_factor <- factor(d$weekday, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
d$month_factor <- factor(d$month, levels = 1:12, labels = month.name)
events <- read.csv('output/ols_baseline/event_dictionary.csv')
check(all(events$term %in% names(d)), 'Missing existing event indicators')
for (j in seq_len(nrow(events))) check(all(d[[events$term[j]]] == as.integer(d$event_name_1 %in% events$event[j] | d$event_name_2 %in% events$event[j])), 'Event indicator mismatch')

# Calendar windows describe known dates, not observed sales outcomes.
holidays <- c('Christmas', 'Thanksgiving', 'Easter', 'IndependenceDay', 'LaborDay', 'NewYear')
windows <- character()
window_audit <- list()
for (holiday in holidays) {
  dates <- calendar$date[calendar$event_name_1 %in% holiday | calendar$event_name_2 %in% holiday]
  for (side in c('before', 'after')) {
    term <- paste0('window_', holiday, '_', side)
    offsets <- if (side == 'before') -3:-1 else 1:3
    active_dates <- as.Date(unlist(lapply(offsets, function(k) dates + k)), origin = '1970-01-01')
    d[[term]] <- as.integer(d$date %in% active_dates)
    independent <- vapply(d$date, function(day) any(as.numeric(day - dates) %in% offsets), logical(1))
    check(identical(d[[term]], as.integer(independent)), 'Holiday window alignment failed')
    windows <- c(windows, term)
    window_audit[[term]] <- data.frame(term, active_store_days = sum(d[[term]]))
  }
}
d <- d %>% group_by(store_id) %>% mutate(lag_1 = lag(total_quantity, 1), lag_7 = lag(total_quantity, 7)) %>% ungroup()
for (s in stores) {
  z <- filter(d, store_id == s)
  check(nrow(z) == 1941 && all(diff(z$date) == 1), 'Incomplete date coverage')
  check(identical(z$lag_1[-1], head(z$total_quantity, -1)) && identical(z$lag_7[-(1:7)], head(z$total_quantity, -7)), 'Lag alignment failed')
}
check(identical(d$total_quantity, original$total_quantity) && identical(d$price_imputed, original$price_imputed), 'Source quantities or price flags changed')
cutoff <- as.Date('2016-04-25')
check(all(d$sample == ifelse(d$date < cutoff, 'train', 'test')), 'Saved split mismatch')
base_terms <- c('weighted_price', 'weekday_factor', 'month_factor', events$term)
holiday_terms <- c(base_terms, windows)
lag_terms <- c(holiday_terms, 'lag_1', 'lag_7')
formulas <- list(Benchmark = reformulate(base_terms, 'total_quantity'),
  Holiday = reformulate(holiday_terms, 'total_quantity'), Lagged = reformulate(lag_terms, 'total_quantity'))
tree_formula <- reformulate(lag_terms)
matrix_for <- function(x) model.matrix(tree_formula, x)[, -1, drop = FALSE]

# A fixed-origin recursion never reads the test target or precomputed test lags.
recursive <- function(history, future, predict_one) {
  values <- history$total_quantity
  result <- numeric(nrow(future))
  for (i in seq_len(nrow(future))) {
    row <- future[i, , drop = FALSE]
    row$lag_1 <- tail(values, 1)
    row$lag_7 <- values[length(values) - 6]
    result[i] <- as.numeric(predict_one(row))
    values <- c(values, result[i])
  }
  check(all(is.finite(result)), 'Nonfinite recursive prediction')
  result
}
toy <- recursive(data.frame(total_quantity = 1:7), data.frame(x = 1:3), function(row) row$lag_1 + row$lag_7)
check(identical(toy, c(8, 10, 13)), 'Hand-calculated recursion test failed')
fit_model <- function(kind, train, config = NULL) {
  if (kind %in% c('Lagged', 'LightGBM')) train <- filter(train, !is.na(lag_7))
  if (kind != 'LightGBM') {
    fit <- lm(formulas[[kind]], train, na.action = na.fail)
    check(fit$rank == ncol(model.matrix(fit)) && all(is.finite(coef(fit))), paste('Unidentified OLS terms:', kind))
    return(fit)
  }
  params <- list(objective = 'regression', learning_rate = 0.05, num_leaves = config$leaves,
    min_data_in_leaf = config$min_leaf, verbosity = -1, seed = 491,
    deterministic = TRUE, force_col_wise = TRUE, num_threads = 1)
  lightgbm::lgb.train(params, lightgbm::lgb.Dataset(matrix_for(train), label = train$total_quantity),
    nrounds = config$rounds)
}
forecast <- function(fit, kind, train, test) {
  if (kind == 'LightGBM') return(recursive(train, test, function(row) predict(fit, matrix_for(row))))
  if (kind == 'Lagged') return(recursive(train, test, function(row) predict(fit, row)))
  as.numeric(predict(fit, test))
}
prediction_rows <- function(s, kind, test, pred) data.frame(store_id = s, model = kind,
  date = test$date, actual = test$total_quantity, predicted = pred,
  error = test$total_quantity - pred, negative_prediction = pred < 0)
score <- function(x) x %>% summarise(n = n(), RMSE = sqrt(mean(error^2)),
  MAE = mean(abs(error)), mean_error = mean(error), .groups = 'drop')

# Tune only on three consecutive 28-day blocks within the original training period.
folds <- data.frame(fold = 1:3, start = cutoff - c(84, 56, 28), end = cutoff - c(57, 29, 1))
save_csv(folds, 'folds')
grid <- expand.grid(leaves = c(7, 15), min_leaf = c(20, 50), rounds = c(100, 300, 600))
grid$config <- seq_len(nrow(grid))
cv <- list(); index <- 0L
for (f in 1:3) {
  message('Validation fold ', f, ' of 3')
  for (s in stores) {
    train <- filter(d, store_id == s, date < folds$start[f])
    test <- filter(d, store_id == s, date >= folds$start[f], date <= folds$end[f])
    check(nrow(test) == 28, 'Incomplete validation horizon')
    for (kind in c('Benchmark', 'Holiday', 'Lagged', 'LightGBM')) {
      candidates <- if (kind == 'LightGBM') seq_len(nrow(grid)) else 0L
      for (g in candidates) {
        fit <- fit_model(kind, train, if (g > 0) grid[g, ] else NULL)
        pred <- forecast(fit, kind, train, test)
        index <- index + 1L
        cv[[index]] <- transform(prediction_rows(s, kind, test, pred), fold = f, config = g)
      }
    }
  }
}
cv <- bind_rows(cv)
tuning <- cv %>% filter(model == 'LightGBM') %>% group_by(config) %>% score() %>%
  left_join(grid, by = 'config') %>% arrange(RMSE, leaves, rounds, desc(min_leaf))
best <- tuning[1, ]
save_csv(tuning, 'tuning_scores')
save_csv(cv, 'validation_predictions')
save_csv(cv %>% filter(model != 'LightGBM' | config == best$config) %>% group_by(model, fold) %>% score(), 'validation_scores')
save_csv(best, 'selected_configuration')
message('Selected LightGBM configuration: ', best$config)

# Final evaluation; preserve the saved benchmark rather than replacing its results.
baseline <- read.csv('output/ols_baseline/predictions.csv')
baseline$date <- as.Date(baseline$date)
predictions <- list(transform(baseline[, c('store_id', 'date', 'actual', 'predicted', 'error', 'negative_prediction')], model = 'Benchmark'))
coefs <- fits <- timings <- importance <- sensitivity <- list()
models <- list()
for (s in stores) {
  message('Final models: ', s)
  train <- filter(d, store_id == s, date < cutoff)
  test <- filter(d, store_id == s, date >= cutoff)
  check(nrow(train) == 1913 && nrow(test) == 28, 'Incorrect split')
  for (kind in c('Benchmark', 'Holiday', 'Lagged', 'LightGBM')) {
    key <- paste(s, kind, sep = '_')
    start <- proc.time()[['elapsed']]
    fit <- fit_model(kind, train, best)
    fit_time <- proc.time()[['elapsed']] - start
    start <- proc.time()[['elapsed']]
    pred <- forecast(fit, kind, train, test)
    prediction_time <- proc.time()[['elapsed']] - start
    poisoned <- test
    poisoned$total_quantity <- -999999
    poisoned$lag_1 <- poisoned$lag_7 <- -999999
    check(isTRUE(all.equal(pred, forecast(fit, kind, train, poisoned), tolerance = 0)), 'Holdout target leaked through lags')
    if (kind == 'Benchmark') {
      old <- arrange(filter(baseline, store_id == s), date)
      check(isTRUE(all.equal(pred, old$predicted, tolerance = 1e-9)), 'Benchmark reproduction mismatch')
    } else predictions[[key]] <- prediction_rows(s, kind, test, pred)
    timings[[key]] <- data.frame(store_id = s, model = kind, fit_seconds = fit_time, predict_seconds = prediction_time)
    if (kind == 'LightGBM') {
      lightgbm::lgb.save(fit, file.path(out, 'models', paste0(key, '.txt')))
      importance[[key]] <- transform(lightgbm::lgb.importance(fit), store_id = s)
    } else {
      models[[key]] <- fit
      cm <- as.data.frame(summary(fit)$coefficients)
      names(cm) <- c('estimate', 'std_error', 't_statistic', 'p_value')
      coefs[[key]] <- data.frame(store_id = s, model = kind, term = rownames(cm), cm, row.names = NULL)
      fits[[key]] <- data.frame(store_id = s, model = kind, n_train = nobs(fit), rank = fit$rank,
        r_squared = summary(fit)$r.squared, adjusted_r_squared = summary(fit)$adj.r.squared)
    }
  }
  # Benchmark and holiday OLS refits isolate the seven-row sample difference.
  for (kind in c('Benchmark', 'Holiday')) {
    common <- filter(train, !is.na(lag_7))
    fit <- fit_model(kind, common)
    models[[paste0(s, '_', kind, '_common')]] <- fit
    sensitivity[[paste(s, kind)]] <- prediction_rows(s, paste0(kind, '_common'), test, forecast(fit, kind, common, test))
  }
}
predictions <- bind_rows(predictions) %>% arrange(model, store_id, date)
check(nrow(predictions) == 1120 && all(is.finite(predictions$predicted)) && !anyDuplicated(predictions[c('model', 'store_id', 'date')]), 'Invalid prediction panel')
check(all(count(predictions, model, store_id)$n == 28), 'Missing predictions')
metrics <- bind_rows(predictions %>% group_by(model, store_id) %>% score(),
  predictions %>% group_by(model) %>% score() %>% mutate(store_id = 'Overall'))
metrics <- metrics %>% left_join(select(filter(metrics, model == 'Benchmark'), store_id, benchmark_RMSE = RMSE), by = 'store_id') %>%
  mutate(improvement_percent = 100 * (benchmark_RMSE - RMSE) / benchmark_RMSE)
save_csv(metrics, 'metrics')
save_csv(predictions, 'predictions')
save_csv(bind_rows(coefs), 'coefficients')
save_csv(bind_rows(fits), 'model_fit')
save_csv(bind_rows(timings), 'timings')
save_csv(bind_rows(importance), 'feature_importance')
save_csv(bind_rows(window_audit), 'holiday_windows')
save_csv(d, 'modeling_dataset')
saveRDS(models, file.path(out, 'models', 'ols_models.rds'))
sensitivity <- bind_rows(bind_rows(sensitivity), filter(predictions, model == 'Lagged'))
check(nrow(sensitivity) == 840 && !anyNA(sensitivity) &&
  all(count(sensitivity, model, store_id)$n == 28), 'Invalid common-sample prediction panel')
save_csv(sensitivity, 'common_sample_predictions')
save_csv(bind_rows(sensitivity %>% group_by(model, store_id) %>% score(), sensitivity %>% group_by(model) %>% score() %>% mutate(store_id = 'Overall')), 'common_sample_metrics')

# Recompute all metrics from serialized predictions, independently of stored errors.
saved <- read.csv(file.path(out, 'predictions.csv'))
for (i in seq_len(nrow(metrics))) {
  z <- subset(saved, model == metrics$model[i])
  if (metrics$store_id[i] != 'Overall') z <- subset(z, store_id == metrics$store_id[i])
  err <- z$actual - z$predicted
  check(max(abs(c(sqrt(mean(err^2)), mean(abs(err)), mean(err)) - unlist(metrics[i, c('RMSE', 'MAE', 'mean_error')]))) < 1e-8, 'Serialized metrics mismatch')
}
validation <- data.frame(check = c('Unique modeling rows', 'Stores', 'Dates per store', 'Imputed prices preserved',
  'Validation folds', 'Tuning configurations', 'Final models including benchmark', 'Finite predictions',
  'Negative predictions', 'Calendar windows checked', 'Hand-calculated recursion passed',
  'Holdout target perturbation passed', 'Benchmark reproduction passed', 'Saved metrics independently verified'),
  value = c(nrow(d), length(stores), 1941, sum(d$price_imputed), 3, nrow(grid), 40, nrow(predictions),
    sum(predictions$negative_prediction), 12, 1, 1, 1, 1))
save_csv(validation, 'validation')

# Flat, faceted comparisons keep every series on its own readable panel.
plot_data <- predictions %>% select(store_id, model, date, Actual = actual, Predicted = predicted) %>%
  pivot_longer(c(Actual, Predicted), names_to = 'series', values_to = 'quantity')
plot_data$model <- factor(plot_data$model, c('Benchmark', 'Holiday', 'Lagged', 'LightGBM'))
plot_base <- function(x) ggplot(x, aes(date, quantity, color = series, linetype = series)) +
  geom_line(linewidth = 0.6) + scale_color_manual(values = c(Actual = '#0071ce', Predicted = '#bc4b19')) +
  scale_linetype_manual(values = c(Actual = 'solid', Predicted = 'dashed')) +
  scale_x_date(breaks = seq(cutoff, cutoff + 27, by = '7 days'), date_labels = '%b %d') +
  labs(x = NULL, y = 'Units sold', color = NULL, linetype = NULL,
    caption = 'Retrospective: same-day weighted price uses actual sales composition.') +
  theme_minimal(base_size = 11) + theme(legend.position = 'top', panel.grid.minor = element_blank())
ggsave(file.path(out, 'figures', 'all_models.png'), plot_base(plot_data) + facet_grid(store_id ~ model, scales = 'free_y'), width = 18, height = 23, dpi = 150)
for (s in stores) ggsave(file.path(out, 'figures', paste0(s, '.png')), plot_base(filter(plot_data, store_id == s)) +
  facet_wrap(~model, ncol = 2) + labs(title = paste(s, '- model comparison')), width = 11, height = 7, dpi = 160)
ggsave(file.path(out, 'figures', 'rmse.png'), ggplot(filter(metrics, store_id != 'Overall'), aes(store_id, RMSE, fill = model)) +
  geom_col(position = 'dodge') + scale_fill_manual(values = c(Benchmark = '#0071ce', Holiday = '#bb8500', Lagged = '#32805a', LightGBM = '#9b4869')) +
  labs(x = NULL, y = 'RMSE (units)', fill = NULL) + theme_minimal(base_size = 12) + theme(legend.position = 'top'), width = 11, height = 5, dpi = 160)
writeLines(capture.output(sessionInfo()), file.path(out, 'session_info.txt'))
print(filter(metrics, store_id == 'Overall'))
print(validation)
