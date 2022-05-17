library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)

source("rcpp_implementation.R")

#set.seed(44)

# Simulated
n <- 1000
p <- 4
beta <- c(2, 1, .5, 0)
beta0 <- 0
cov_base <- 0
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
#x[, 3] <- rbinom(n, 1, .5)
#x[, 2] <- x[, 2] - min(x[, 2])
x[, 1] <- round(10*(x[, 1] - min(x[, 1])))
x[, 4] <- rbinom(n, 1, .5)
#lp <- x %*% beta + beta0 + 2 * x[, 1] * x[, 4] - (1+(-2)*(x[, 1] > 40)) * 20 * x[, 2] * (1-x[, 4])
lp <- x %*% beta + beta0 + (1+(-2)*(x[, 1] > 40)) * 10 * x[, 2] * (1-x[, 4])

y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# Ground truth Shapley values
#shapley <- sweep(x, 2, beta, "*")
#inte

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)

# Interaction SHAP
uxg <- xgboost.unify(xg, x)
tsi <- treeshap(uxg, x, interactions = TRUE)

df_ts <- data.frame(age = x[, 1],
                    sex = factor(x[, 4], levels = c(0,  1), labels = c("Female", "Male")),
                    interaction = tsi$interactions[1, 4, ] +  tsi$interactions[4, 1, ])
# ggplot(df_ts, aes(x = age, y = interaction, col = sex)) +
#   geom_point() +
#   scale_colour_manual(values = c("red", "blue")) +
#   theme_bw() +
#   ggtitle("TreeSHAP")

# My SHAP
res <- myshap_rcpp(xg, x)

df_my <- data.frame(age = x[, 1],
                    sex = factor(x[, 4], levels = c(0,  1), labels = c("Female", "Male")),
                    interaction = res$m[, "x1:x4"])
# ggplot(df_my, aes(x = age, y = interaction, col = sex)) +
#   geom_point() +
#   #geom_line() +
#   scale_colour_manual(values = c("red", "blue")) +
#   theme_bw() +
#   ggtitle("Our method")

df <- data.frame(age = x[, 1],
                 sex = factor(x[, 4], levels = c(0,  1), labels = c("Female", "Male")),
                 my_interaction = res$m[, "x1:x4"],
                 ts_interaction = tsi$interactions[1, 4, ] + tsi$interactions[4, 1, ])

ggplot(df, aes(x = age, y = ts_interaction, col = sex)) +
  geom_point() +
  geom_line(aes(y = my_interaction)) +
  scale_colour_manual(values = c("red", "blue")) +
  theme_bw() +
  ylab("Interaction")

# Plot 3-way interaction
df_my <- data.frame(age = x[, 1],
                    sex = factor(x[, 4], levels = c(0,  1), labels = c("Female", "Male")),
                    x2 = factor(x[, 2] > 0),
                    interaction = res$m[, "x1:x2:x4"])

ggplot(df_my, aes(x = age, y = interaction, col = sex, shape = x2)) +
  geom_point() +
  #geom_line() +
  scale_colour_manual(values = c("red", "blue")) +
  theme_bw() +
  ggtitle("Our method")
