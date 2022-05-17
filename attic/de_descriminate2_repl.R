
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)

source("rcpp_implementation.R")

res <- replicate(20, {
  n <- 1000
  beta_x1 <- 2
  beta_x2 <- 2
  beta_race <- 5

  x1 <- rnorm(n)
  x2 <- rnorm(n)
  race <- rbinom(n, 1, .5)

  # x2 correlated with race
  x2[race == 1] <- x2[race == 1] + 0.5

  y <- beta_x1 * x1 + beta_x2 * x2 + beta_race * race + rnorm(n)
  dat <- data.frame(y = y, x1 = x1, x2 = x2, race = race)

  x <- as.matrix(dat[, -1])
  y <- dat[, 1]

  idx <- sample(n, 2/3*n)
  train <- list(x = x[idx, ], y = y[idx])
  test <- list(x = x[-idx, ], y = y[-idx])

  xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)

  # My SHAP
  res <- myshap_rcpp(xg, test$x)

  # Full model decomposition is same as prediction
  pred_full <- predict(xg, test$x)
  mean_pred <- mean(pred_full)

  # Remove race from model
  m_norace <- res$m[, grep("race", colnames(res$m), invert = TRUE)]
  m_reduced <- rowSums(m_norace) + mean_pred
  #plot(m_reduced, predict(xg, test$x))

  # Re-fit without race
  xg_norace <- xgboost(data = train$x[, -3], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
  pred_norace <- predict(xg_norace, test$x[, -3])

  # Equalized odds proposes that the predictor and the protected attribute should be independent, conditional on the outcome.
  # res_full <- lm(Female ~ ., data.frame(pred_full, test$x))$coefficients[c(-1, -2)]
  # res_full <- lm(Female ~ ., data.frame(m_reduced, test$x))$coefficients[c(-1, -2)]
  # res_full <- lm(Female ~ ., data.frame(pred_nosex, test$x))$coefficients[c(-1, -2)]

  # Well-calibrated systems propose that the outcome and protected attribute are independent, conditional on the predictor.
  res_full <- lm(pred_full ~ ., data.frame(pred_full, test$x))$coefficients["race"]
  res_reduced <- lm(m_reduced ~ ., data.frame(m_reduced, test$x))$coefficients["race"]
  res_refit <- lm(pred_norace ~ ., data.frame(pred_norace, test$x))$coefficients["race"]

  c(full = res_full,
    reduced = res_reduced,
    refit = res_refit)
})

boxplot(t(res))
abline(0, 0, col = "red")
