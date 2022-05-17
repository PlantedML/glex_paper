
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)

library(fairml)
data(german.credit)

source("rcpp_implementation.R")

# Prepare data for xgboost
credit <- as.data.frame(lapply(german.credit[, c("Credit_risk", "Gender", "Account_status", "Duration", "Credit_history", "Credit_amount")], as.numeric))

x <- as.matrix(credit[, -1])
y <- credit$Credit_risk - 1

res <- replicate(20, {
  n <- nrow(x)
  idx <- sample(n, 2/3*n)
  train <- list(x = x[idx, ], y = y[idx])
  test <- list(x = x[-idx, ], y = y[-idx])

  xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)

  # My SHAP
  res <- myshap_rcpp(xg, test$x)

  # Full model decomposition is same as prediction
  pred_full <- predict(xg, test$x)
  mean_pred <- mean(pred_full)

  # Remove sex from model
  m_nosex <- res$m[, grep("Gender", colnames(res$m), invert = TRUE)]
  m_reduced <- rowSums(m_nosex) + mean_pred
  #plot(m_reduced, predict(xg, test$x))

  # Re-fit without sex
  xg_nosex <- xgboost(data = train$x[, -1], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
  pred_nosex <- predict(xg_nosex, test$x[, -1])

  # Equalized odds proposes that the predictor and the protected attribute should be independent, conditional on the outcome.
  # res_full <- lm(Female ~ ., data.frame(pred_full, test$x))$coefficients[c(-1, -2)]
  # res_full <- lm(Female ~ ., data.frame(m_reduced, test$x))$coefficients[c(-1, -2)]
  # res_full <- lm(Female ~ ., data.frame(pred_nosex, test$x))$coefficients[c(-1, -2)]

  # Well-calibrated systems propose that the outcome and protected attribute are independent, conditional on the predictor.
  res_full <- lm(pred_full ~ ., data.frame(pred_full, test$x))$coefficients["Gender"]
  res_reduced <- lm(m_reduced ~ ., data.frame(m_reduced, test$x))$coefficients["Gender"]
  res_refit <- lm(pred_nosex ~ ., data.frame(pred_nosex, test$x))$coefficients["Gender"]

  c(full = res_full,
    reduced = res_reduced,
    refit = res_refit)
})

boxplot(t(res))
abline(0, 0, col = "red")
