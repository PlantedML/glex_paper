library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)

source("rcpp_implementation.R")

#set.seed(44)

# Simulated
n <- 1000
p <- 3
beta <- c(1, 1, -2)
beta0 <- 0
cov_base <- 0#.3
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 - 2* x[, 1] * x[, 2] * x[, 3]
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)


# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)

# My SHAP
res <- myshap_rcpp(xg, x)

# Interaction TreeSHAP
uxg <- xgboost.unify(xg, x)
tsi <- treeshap(uxg, x, interactions = TRUE)


# SHAP
df <- rbind(data.frame(Variable = "x1", x = x[, 1], y = res$shap[, "x1"], z = 0),
            data.frame(Variable = "x2", x = x[, 2], y = res$shap[, "x2"], z = 0),
            data.frame(Variable = "x3", x = x[, 3], y = res$shap[, "x3"], z = 0))
p1 <- ggplot(df, aes(x = x, y = y)) +
  facet_wrap(~ Variable) +
  geom_point() +
  #scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()

# TreeSHAP interactions
df <- rbind(data.frame(Variable = "x1", x = x[, 1], y = tsi$interactions[1, 1, ], z = 0),
            data.frame(Variable = "x2", x = x[, 2], y = tsi$interactions[2, 2, ], z = 0),
            data.frame(Variable = "x3", x = x[, 3], y = tsi$interactions[3, 3, ], z = 0))
p2 <- ggplot(df, aes(x = x, y = y)) +
  facet_wrap(~ Variable) +
  geom_point() +
  #scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()

# m
df <- rbind(data.frame(Variable = "x1", x = x[, 1], y = res$m[, "x1"], z = 0),
            data.frame(Variable = "x2", x = x[, 2], y = res$m[, "x2"], z = 0),
            data.frame(Variable = "x3", x = x[, 3], y = res$m[, "x3"], z = 0))
p3 <- ggplot(df, aes(x = x, y = y)) +
  facet_wrap(~ Variable) +
  geom_point() +
  #scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()

require(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
