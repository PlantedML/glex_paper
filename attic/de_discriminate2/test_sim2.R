library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(shapdecomp)

set.seed(42)

n <- 10000

aa <- replicate(10, {
  sex <- rbinom(n, 1, .5) # 0=female, 1=male
  hours <- 30 + 10 * sex + 1*rnorm(n) # men work more hours
  y <- 20 * sex + 1 * hours + rnorm(n)
  dat <- data.frame(y = y, sex = sex, hours = hours)
  x <- as.matrix(dat[, -1])

  idx <- sample(n, 2/3*n)
  train <- list(x = x[idx, ], y = y[idx])
  test <- list(x = x[-idx, ], y = y[-idx])

  # test$x <- rbind(matrix(c(0, 30, 0), nrow = 1),
  #                 rbind(matrix(c(1, 30, 0), nrow = 1),
  #                       test$x))

  # Fit model
  xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 4, eta = .1), nrounds = 50)
  pred_full <- predict(xg, test$x)
  mean_pred <- mean(pred_full)

  # Compute decomposition
  res <- shapdecomp(xg, test$x)

  # Re-fit without sex
  xg_refit <- xgboost(data = train$x[, colnames(train$x) != "sex", drop = FALSE], label = train$y,
                      params = list(max.depth = 4, eta = .1), nrounds = 50, verbose = FALSE)
  pred_refit <- predict(xg_refit, test$x[, colnames(test$x) != "sex", drop = FALSE])

  # Remove sex from decomposed model
  #m_reduced <- res$m[, colnames(res$m) != "sex", drop = FALSE]
  m_reduced <- res$m[, grep("sex", colnames(res$m), invert = TRUE), drop = FALSE]
  pred_reduced <- rowSums(m_reduced) + mean_pred

  female_idx <- test$x[, "sex"] == 0
  male_idx <- test$x[, "sex"] == 1

  dif_full <- mean(pred_full[male_idx]) - mean(pred_full[female_idx])
  dif_refit <- mean(pred_refit[male_idx]) - mean(pred_refit[female_idx])
  dif_reduced <- mean(pred_reduced[male_idx]) - mean(pred_reduced[female_idx])

  c(full = dif_full,
    refit = dif_refit,
    reduced = dif_reduced)
})

boxplot(t(aa))
rowMeans(aa)
