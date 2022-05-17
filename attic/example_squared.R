library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(gridExtra)
library(shapdecomp)

#set.seed(44)

# Simulated
n <- 1000
p <- 2
beta0 <- 0
cov_base <- 0
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- beta0 + x[, 1]^2 - x[, 2]^2 + 1*x[, 1] * x[, 2]
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 6, eta = .5), nrounds = 10)

# My SHAP
res <- shapdecomp(xg, x)

# Plot SHAP
df <- data.frame(x1 = x[, 1],
                 shap1 = res$shap[, "x1"],
                 x2 = x[, 2],
                 shap2 = res$shap[, "x2"])
p1 <- ggplot(df, aes(x = x1, y = shap1)) +
  geom_point() +
  #geom_line() +
  theme_bw()
p2 <- ggplot(df, aes(x = x2, y = shap1)) +
  geom_point() +
  #geom_line() +
  theme_bw()
grid.arrange(p1, p2, ncol=2)

# Plot m's
df <- data.frame(x1 = x[, 1],
                 m1 = res$m[, "x1"],
                 x2 = x[, 2],
                 m2 = res$m[, "x2"],
                 x1x2 = x[, 1]*x[, 2],
                 m12 = res$m[, "x1:x2"])
p1 <- ggplot(df, aes(x = x1, y = m1)) +
  #geom_point() +
  geom_line() +
  theme_bw()
p2 <- ggplot(df, aes(x = x2, y = m2)) +
  #geom_point() +
  geom_line() +
  theme_bw()
p3 <- ggplot(df, aes(x = x1, y = x2, col = m12)) +
  geom_point() +
  #geom_line() +
  scale_color_viridis_b() +
  theme_bw()
grid.arrange(p1, p2, p3, ncol=3)
