library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(shapdecomp)

#set.seed(42)

n <- 1000
beta_x1 <- 2
beta_x2 <- 2
beta_race <- 5

x1 <- rnorm(n)
x2 <- rnorm(n)
race <- rbinom(n, 1, .5)

# x2 correlated with race
x2_cor <- x2
x2_cor[race == 1] <- x2_cor[race == 1] + 0.5


# Model without correlation -----------------------------------------------
y <- beta_x1 * x1 + beta_x2 * x2 + beta_race * race + rnorm(n)
dat <- data.frame(y = y, x1 = x1, x2 = x2, race = race)
x <- as.matrix(dat[, -1])
y <- dat[, 1]

# Fit model
xg <- xgboost(data = x, label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_full <- predict(xg, x)
mean_pred <- mean(pred_full)

# Refit without race
xg_refit <- xgboost(data = x[, -3], label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_refit <- predict(xg_refit, x[, -3])

# SHAP decomposition and remove race from model
res <- shapdecomp(xg, x)
m_norace <- res$m[, grep("race", colnames(res$m), invert = TRUE)]
pred_reduced <- rowSums(m_norace) + mean_pred

# Model with correlation -----------------------------------------------
y <- beta_x1 * x1 + beta_x2 * x2_cor + beta_race * race + rnorm(n)
dat <- data.frame(y = y, x1 = x1, x2 = x2_cor, race = race)
x <- as.matrix(dat[, -1])
y <- dat[, 1]

# Fit model
xg <- xgboost(data = x, label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)

# Refit without race
xg_refit <- xgboost(data = x[, -3], label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_refit_cor <- predict(xg_refit, x[, -3])

# SHAP decomposition and remove race from model
res_cor <- shapdecomp(xg, x)
m_norace_cor <- res_cor$m[, grep("race", colnames(res_cor$m), invert = TRUE)]
pred_reduced_cor <- rowSums(m_norace_cor) + mean_pred

#plot(test$x[, "x1"], res_nocor$m[, "x1"])

plot(res$m[, "x1"], res_cor$m[, "x1"])
plot(res$m[, "x2"], res_cor$m[, "x2"], col = race+1)

plot(pred_reduced, pred_reduced_cor)
plot(pred_refit, pred_refit_cor)

