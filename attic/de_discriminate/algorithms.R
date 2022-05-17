
library(data.table)
library(xgboost)
library(cpi)
library(mlr3)
library(mlr3learners)
library(shapdecomp)

shap_decomp <- function(data, job, instance, nrounds, ...) {
  x <- instance$x
  y <- instance$y

  # Train/test split
  n <- nrow(x)
  idx <- sample(n, 2/3*n)
  train <- list(x = x[idx, ], y = y[idx])
  test <- list(x = x[-idx, ], y = y[-idx])

  xg <- xgboost(data = train$x, label = train$y, params = list(...), nrounds = nrounds, verbose = FALSE)

  # Compute decomposition
  res <- shapdecomp(xg, test$x)

  # Full model decomposition is same as prediction
  pred_full <- predict(xg, test$x)
  mean_pred <- mean(pred_full)

  # Remove protected attribute from decomposed model
  m_reduced <- res$m[, grep("protected", colnames(res$m), invert = TRUE)]
  pred_reduced <- rowSums(m_reduced) + mean_pred

  # Re-fit without protected attribute
  xg_refit <- xgboost(data = train$x[, colnames(train$x) != "protected"], label = train$y,
                      params = list(...), nrounds = nrounds, verbose = FALSE)
  pred_refit <- predict(xg_refit, test$x[, colnames(test$x) != "protected"])

  # Outcome and protected attribute should be independent, conditional on the other predictors
  # Test with CPI
  mytask <- as_task_regr(data.frame(pred_full, test$x), target = "pred_full")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", nrounds = nrounds, ...),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_full <- res_cpi[res_cpi$Variable == "protected", "statistic"]

  mytask <- as_task_regr(data.frame(pred_reduced, test$x), target = "pred_reduced")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", nrounds = nrounds, ...),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_reduced <- res_cpi[res_cpi$Variable == "protected", "statistic"]

  mytask <- as_task_regr(data.frame(pred_refit, test$x), target = "pred_refit")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", nrounds = nrounds, ...),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_refit <- res_cpi[res_cpi$Variable == "protected", "statistic"]

  c(full = res_full,
    reduced = res_reduced,
    refit = res_refit)
}
