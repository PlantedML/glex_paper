
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)

source("rcpp_implementation.R")

compas <- fread("compas.csv", select = c(2:8), stringsAsFactors = TRUE)

# Convert to numeric
compas[, Two_yr_Recidivism := 1*(Two_yr_Recidivism == "yes")]
compas[, Age_Above_FourtyFive  := 1*(Age_Above_FourtyFive  == "yes")]
compas[, Age_Below_TwentyFive  := 1*(Age_Below_TwentyFive  == "yes")]
compas[, Female  := 1*(Female  == "Female")]
compas[, Misdemeanor  := 1*(Misdemeanor  == "yes")]
compas[, ethnicity := as.numeric(ethnicity)]

# Prepare data for xgboost
x <- as.matrix(compas[, -1])
y <- compas$Two_yr_Recidivism

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
  m_nosex <- res$m[, grep("Female", colnames(res$m), invert = TRUE)]
  m_reduced <- rowSums(m_nosex) + mean_pred
  #plot(m_reduced, predict(xg, test$x))

  # Re-fit without sex
  xg_nosex <- xgboost(data = train$x[, -4], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
  pred_nosex <- predict(xg_nosex, test$x[, -4])

  # Outcome and protected attribute should be independent, conditional on the other predictors
  res_full <- lm(pred_full ~ ., data.frame(pred_full, test$x))$coefficients["Female"]
  res_reduced <- lm(m_reduced ~ ., data.frame(m_reduced, test$x))$coefficients["Female"]
  res_refit <- lm(pred_nosex ~ ., data.frame(pred_nosex, test$x))$coefficients["Female"]

  c(full = res_full,
    reduced = res_reduced,
    refit = res_refit)
})

boxplot(t(res))
abline(0, 0, col = "red")
