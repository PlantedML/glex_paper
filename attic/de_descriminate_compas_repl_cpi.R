
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)
library(cpi)
library(mlr3)
library(mlr3learners)

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

res <- replicate(100, {
  n <- nrow(x)
  idx <- sample(n, 2/3*n)
  train <- list(x = x[idx, ], y = y[idx])
  test <- list(x = x[-idx, ], y = y[-idx])

  xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)

  # Compute decomposition
  res <- myshap_rcpp(xg, test$x)

  # Full model decomposition is same as prediction
  pred_full <- predict(xg, test$x)
  mean_pred <- mean(pred_full)

  # Remove sex from model
  m_nosex <- res$m[, grep("Female", colnames(res$m), invert = TRUE)]
  m_reduced <- rowSums(m_nosex) + mean_pred

  # Re-fit without sex
  xg_nosex <- xgboost(data = train$x[, -4], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
  pred_nosex <- predict(xg_nosex, test$x[, -4])

  # Outcome and protected attribute should be independent, conditional on the other predictors
  mytask <- as_task_regr(data.frame(pred_full, test$x), target = "pred_full")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", max_depth = 5, eta = .5, nrounds = 10),
      resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_full <- res_cpi[res_cpi$Variable == "Female", "statistic"]

  mytask <- as_task_regr(data.frame(m_reduced, test$x), target = "m_reduced")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", max_depth = 5, eta = .5, nrounds = 10),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_reduced <- res_cpi[res_cpi$Variable == "Female", "statistic"]

  mytask <- as_task_regr(data.frame(pred_nosex, test$x), target = "pred_nosex")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", max_depth = 5, eta = .5, nrounds = 10),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_refit <- res_cpi[res_cpi$Variable == "Female", "statistic"]

  c(full = res_full,
    reduced = res_reduced,
    refit = res_refit)
})

df <- melt(res)
colnames(df) <- c("Model", "repl", "Statistic")
ggplot(df, aes(x = Model, y = Statistic)) +
  geom_boxplot(fill = "lightgrey") +
  geom_hline(yintercept = 0, col = "red") +
  theme_bw()

saveRDS(df, "de_descriminate_compas.Rds")

ggsave("de_descriminate_compas.pdf")

