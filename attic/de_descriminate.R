
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)

source("rcpp_implementation.R")

#set.seed(42)

n <- 10000
beta_x1 <- 2
beta_x2 <- 2
beta_race <- 5

x1 <- rnorm(n)
x2 <- rnorm(n)
race <- rbinom(n, 1, .5)

y <- beta_x1 * x1 + beta_x2 * x2 + beta_race * race + rnorm(n)
dat <- data.frame(y = y, x1 = x1, x2 = x2, race = race)

x <- as.matrix(dat[, -1])
y <- dat[, 1]

idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 10, eta = .5), nrounds = 10)

# My SHAP
res <- myshap_rcpp(xg, test$x)

# Interaction TreeSHAP
#uxg <- xgboost.unify(xg, x)
#tsi <- treeshap(uxg, x, interactions = TRUE)

# Plot SHAP
df <- rbind(data.frame(Variable = "x1", x = test$x[, 1], y = res$shap[, "x1"]),
            data.frame(Variable = "x2", x = test$x[, 2], y = res$shap[, "x2"]),
            data.frame(Variable = "race", x = test$x[, 3], y = res$shap[, "race"]))
ggplot(df, aes(x = x, y = y)) +
  facet_wrap(~ Variable) +
  geom_point() +
  #scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()

# Plot m
df <- rbind(data.frame(Variable = "x1", x = test$x[, 1], y = res$m[, "x1"]),
            data.frame(Variable = "x2", x = test$x[, 2], y = res$m[, "x2"]),
            data.frame(Variable = "race", x = test$x[, 3], y = res$m[, "race"]))
ggplot(df, aes(x = x, y = y)) +
  facet_wrap(~ Variable) +
  geom_point() +
  #scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()

# Full model decomposition is same as prediction
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)
m_full <- rowSums(res$m) + mean_pred
plot(m_full, predict(xg, test$x))

# Remove race from model
m_norace <- res$m[, grep("race", colnames(res$m), invert = TRUE)]
m_reduced <- rowSums(m_norace) + mean_pred
plot(m_reduced, predict(xg, test$x))

# Re-fit without race
xg_norace <- xgboost(data = train$x[, -3], label = train$y, params = list(max_depth = 10, eta = .5), nrounds = 10)
pred_norace <- predict(xg_norace, test$x[, -3])

df <- rbind(data.frame(model = "original", race = factor(test$x[, 3]), pred = pred_full),
            data.frame(model = "refit", race = factor(test$x[, 3]), pred = pred_norace),
            data.frame(model = "reduce", race = factor(test$x[, 3]), pred = m_reduced))
df$model <- factor(df$model, levels = c("original", "refit", "reduce"), labels = c("original", "refit", "reduce"))
ggplot(df, aes(x = model, y = pred, fill = race)) +
  geom_boxplot() +
  theme_bw()

# plot(pred_norace, m_reduced, col = x[, 3]+1)
#
# plot(pred_norace, col = x[, 3]+1)
# plot(m_reduced, col = x[, 3]+1)

mean((m_reduced - test$y)^2)
mean((pred_norace - test$y)^2)

cor(pred_full, test$x[, 3])
cor(m_reduced, test$x[, 3])
cor(pred_norace, test$x[, 3])

#Equalized odds proposes that the predictor and the protected attribute should be independent, conditional on the outcome.
lm(test$x[, "x2"] ~ test$x[, "race"] + pred_full)
lm(test$x[, "x2"] ~ test$x[, "race"] + m_reduced)
lm(test$x[, "x2"] ~ test$x[, "race"] + pred_norace)

