
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)
library(cpi)
library(mlr3)
library(mlr3learners)

library(fairml)
data(adult)

source("rcpp_implementation.R")

# Prepare data for xgboost
adult <- as.data.frame(lapply(adult, as.numeric))

x <- as.matrix(adult[, -14])
y <- adult$income

res <- replicate(10, {
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
  m_nosex <- res$m[, grep("sex", colnames(res$m), invert = TRUE)]
  m_reduced <- rowSums(m_nosex) + mean_pred

  # Re-fit without sex
  xg_nosex <- xgboost(data = train$x[, -9], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
  pred_nosex <- predict(xg_nosex, test$x[, -9])

  # Outcome and protected attribute should be independent, conditional on the other predictors
  mytask <- as_task_regr(data.frame(pred_full, test$x), target = "pred_full")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", max_depth = 5, eta = .5, nrounds = 10),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_full <- res_cpi[res_cpi$Variable == "sex", "statistic"]

  mytask <- as_task_regr(data.frame(m_reduced, test$x), target = "m_reduced")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", max_depth = 5, eta = .5, nrounds = 10),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_reduced <- res_cpi[res_cpi$Variable == "sex", "statistic"]

  mytask <- as_task_regr(data.frame(pred_nosex, test$x), target = "pred_nosex")
  res_cpi <- cpi(task = mytask, learner = lrn("regr.xgboost", max_depth = 5, eta = .5, nrounds = 10),
                 resampling = rsmp("holdout"), measure = msr("regr.mse"))
  res_refit <- res_cpi[res_cpi$Variable == "sex", "statistic"]

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

saveRDS(df, "de_descriminate_adult.Rds")

ggsave("de_descriminate_adult.pdf")
