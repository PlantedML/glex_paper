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
x2[race == 1] <- x2[race == 1] + 0.5

y <- beta_x1 * x1 + beta_x2 * x2 + beta_race * race + rnorm(n)
dat <- data.frame(y = y, x1 = x1, x2 = x2, race = race)
x <- as.matrix(dat[, -1])
y <- dat[, 1]

idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

# Fit model
xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)

# Refit without race
xg_refit <- xgboost(data = train$x[, -3], label = train$y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_refit <- predict(xg_refit, test$x[, -3])

# SHAP decomposition and remove race from model
res <- shapdecomp(xg, test$x)
m_norace <- res$m[, grep("race", colnames(res$m), invert = TRUE)]
pred_reduced <- rowSums(m_norace) + mean_pred

# Fall 1:
# (a) fitte alle features mit response refitted,
# (b) fitte protected variable mit response refitted.
# In diesem Fall sollte (b) eine kleinere ( ~0) importance zeigen.
(lm(pred ~ ., data.frame(pred = pred_refit, test$x)))
(lm(pred ~ ., data.frame(pred = pred_refit, race = test$x[, "race"])))

# Fall 2:
# (a) fitte alle features mit response reduced,
# (b) fitte protected variable mit response reduced.
# In diesem Fall sollte variable importance von (a) und (b) vergleichbar sein.
(lm(pred ~ ., data.frame(pred = pred_reduced, test$x)))
(lm(pred ~ ., data.frame(pred = pred_reduced, race = test$x[, "race"])))

# Fall 1:
# (a) fitte alle features mit response refitted,
# (b) fitte protected variable mit response refitted.
# In diesem Fall sollte (b) eine kleinere ( ~0) importance zeigen.
ranger(pred ~ ., data.frame(pred = pred_refit, test$x), importance = "permutation")$variable.importance
ranger(pred ~ ., data.frame(pred = pred_refit, race = test$x[, "race"]), importance = "permutation")$variable.importance

# Fall 2:
# (a) fitte alle features mit response reduced,
# (b) fitte protected variable mit response reduced.
# In diesem Fall sollte variable importance von (a) und (b) vergleichbar sein.
ranger(pred ~ ., data.frame(pred = pred_reduced, test$x), importance = "permutation")$variable.importance
ranger(pred ~ ., data.frame(pred = pred_reduced, race = test$x[, "race"]), importance = "permutation")$variable.importance

