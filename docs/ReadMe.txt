"calendar.csv" - Contains information about the date the sales refer to, the existence of special days and SNAP activities
"sell_prices.csv" - Contains information about the sell prices of the products for each store and week
"sales_train_validation.csv" - The train set provided during the validation phase of the competition (public leaderboard)
"sales_test_validation.csv" - The test set used for the validation phase of the competition (public leaderboard)
"sales_train_evaluation.csv" - The train set provided during the evaluation phase of the competition (private leaderboard)
"sales_test_evaluation.csv" - The test set used for the evaluation phase of the competition (private leaderboard)

"weights_validation.csv" - The weights used for computing WRMSSE for the validation phase of the competition (public leaderboard)
"weights_evaluation.csv" - The weights used for computing WRMSSE for the evaluation phase of the competition (private leaderboard)

"Estimate weights.R" - R code for determining the weights used for computing WRMSSE for the validation or evaluation phase of the competition
"Estimate WRMSSE.R" - R code for computing WRMSSE for the validation or evaluation phase of the competition, using the weights determined earlier. As a toy example, the Naive method is considered.