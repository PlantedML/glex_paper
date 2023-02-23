library(mvtnorm)
library(xgboost)
# library(treeshap)
library(data.table)
library(ggplot2)
library(gridExtra)
# library(shapdecomp)

source("rpf.R")
source("shap_rpf.R")

set.seed(2022)

max_interaction = 2
ntrees = 50

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

res_rpf <- rpf(X=x, Y=y, max_interaction = max_interaction, ntrees = ntrees, splits = 70)

X<-as.matrix(x)

res_shap <- shap_rpf(res_rpf, X)

res = pred_shap_rpf(x,res_shap, shap=TRUE)

res_x0 = res[,4]
res_x1 = res[,2]
res_x2 = res[,3]
res_x12 = res[,5]

# Plot SHAP
df <- data.frame(x1 = x[, 1],
                 shap1 = res[, 6],
                 x2 = x[, 2],
                 shap2 = res[, 7])
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
#grid.arrange(p_s1, p_s2, ncol=2)

# Plot m's
df <- data.frame(x1 = x[, 1],
                 m1 = res_x1,
                 x2 = x[, 2],
                 m2 = res_x2,
                 x1x2 = x[, 1]*x[, 2],
                 m12 = res_x12)

df <- df[df$x1 <= 2 & df$x1 >= -2 & df$x2 <= 2 & df$x2 >= -2, ]
p_m1 <- ggplot(df, aes(x = x1, y = m1)) +
  #  geom_point() +
  geom_line() +
  geom_abline(intercept = -0.6, slope = 1, col = "red") +
  theme_bw()
p_m2 <- ggplot(df, aes(x = x2, y = m2)) +
  #  geom_point() +
  geom_line() +
  geom_abline(intercept = -0.6, slope = 1, col = "red") +
  theme_bw()
p_m3 <- ggplot(df, aes(x = x1, y = x2, col = m12)) +
  geom_point() +
  scale_color_viridis_b() +
  theme_bw()
grid.arrange(p_m1, p_m2, p_m3, ncol=3)

# Plot everythin together
blank <- grid::grid.rect(gp = grid::gpar(col = "white"))
p <- arrangeGrob(p_s1, p_s2, blank, p_m1, p_m2, p_m3, ncol=3, widths = c(.3, .3, .4))

# plot(p)

ggsave("simple_example.png", plot = p, width = 10, height = 6)