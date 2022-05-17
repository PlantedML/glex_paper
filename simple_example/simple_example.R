library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(shapdecomp)
library(cowplot)

set.seed(2022)

# Simulated
n <- 10000
p <- 2
beta <- c(1, 1)
beta0 <- 0
cov_base <- 0.3
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 20)

# SHAP decomposition
res <- shapdecomp(xg, x)

# Plot SHAP
df <- data.frame(x1 = x[, 1],
                 shap1 = res$shap[, "x1"],
                 x2 = x[, 2],
                 shap2 = res$shap[, "x2"])
df <- df[df$x1 <= 2 & df$x1 >= -2 & df$x2 <= 2 & df$x2 >= -2, ]
p_s1 <- ggplot(df, aes(x = x1, y = shap1)) +
  geom_point() +
  #geom_abline(intercept = 0, slope = 1, col = "red") +
  theme_bw() +
  xlab(expression(italic(X[1]))) +
  ylab(expression(SHAP~value~italic(phi[~1])))
p_s2 <- ggplot(df, aes(x = x2, y = shap2)) +
  geom_point() +
  #geom_abline(intercept = 0, slope = 1, col = "red") +
  theme_bw() +
  xlab(expression(italic(X[2]))) +
  ylab(expression(SHAP~value~italic(phi[~2])))

# Plot m's
df <- data.frame(x1 = x[, 1],
                 m1 = res$m[, "x1"],
                 x2 = x[, 2],
                 m2 = res$m[, "x2"],
                 x1x2 = x[, 1]*x[, 2],
                 m12 = res$m[, "x1:x2"])
df <- df[df$x1 <= 2 & df$x1 >= -2 & df$x2 <= 2 & df$x2 >= -2, ]
p_m1 <- ggplot(df, aes(x = x1, y = m1)) +
  geom_line() +
  #geom_abline(intercept = -.6, slope = 1, col = "red") +
  theme_bw() +
  xlab(expression(italic(X[1]))) +
  ylab(expression(Decomposition~italic(m[1])))
p_m2 <- ggplot(df, aes(x = x2, y = m2)) +
  geom_line() +
  #geom_abline(intercept = -.6, slope = 1, col = "red") +
  theme_bw() +
  xlab(expression(italic(X[2]))) +
  ylab(expression(Decomposition~italic(m[2])))
p_m3 <- ggplot(df, aes(x = x1, y = x2, col = m12)) +
  geom_point() +
  scale_color_viridis_b(name = expression(italic(m[12]))) +
  theme_bw() +
  xlab(expression(italic(X[1]))) +
  ylab(expression(italic(X[2])))

# Plot everything together
blank <- grid::grid.rect(gp = grid::gpar(col = "white"))
plot_grid(p_s1, p_s2, blank, p_m1, p_m2, p_m3, ncol=3, rel_widths = c(.3, .3, .4))
ggsave("simple_example.pdf", width = 10, height = 5.5)
ggsave("simple_example.png", width = 10, height = 5.5)
