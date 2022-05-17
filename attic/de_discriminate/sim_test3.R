library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(shapdecomp)

#set.seed(42)

n <- 1000
beta_x1 <- 2
beta_x2 <- 2
beta_race <- 1#2

x1 <- rnorm(n)
x2 <- rnorm(n)
race <- rbinom(n, 1, .5)

# x2 correlated with race
#x2[race == 1] <- x2[race == 1] + 5

y <- beta_x1 * x1 + beta_x2 * x2 + beta_race * race + rnorm(n) + 4 * x2 * race
dat <- data.frame(y = y, x1 = x1, x2 = x2, race = race)
x <- as.matrix(dat[, -1])
y <- dat[, 1]

idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

# Fit model
xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 4, eta = .5), nrounds = 10)
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)

# Refit without race
xg_refit <- xgboost(data = train$x[, -3], label = train$y, params = list(max_depth = 4, eta = .5), nrounds = 10)
pred_refit <- predict(xg_refit, test$x[, -3])

# SHAP decomposition and remove race from model
res <- shapdecomp(xg, test$x)
m_norace <- res$m[, grep("race", colnames(res$m), invert = TRUE)]
pred_reduced <- rowSums(m_norace) + mean_pred

m_norace2 <- res$m[, colnames(res$m) != "race"]
pred_reduced2 <- rowSums(m_norace2) + mean_pred

# MSE
mse_original <- mean((test$y - pred_full)^2)
mse_refit <- mean((test$y - pred_refit)^2)
mse_reduced <- mean((test$y - pred_reduced)^2)
mse_reduced2 <- mean((test$y - pred_reduced2)^2)

mse_original
mse_refit
mse_reduced
mse_reduced2

plot(test$x[, "x1"], res$m[, "x1"])
abline(0, 2, col = "red")
plot(test$x[, "x2"], res$m[, "x2"])
abline(0, 2, col = "red")

