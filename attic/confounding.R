
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)

source("rcpp_implementation.R")

#set.seed(42)

# x <- z -> y
n <- 1000
beta_z_x <- 2
beta_z_y <- 2

z <- rnorm(n)
x <- beta_z_x * z + rnorm(n)
y <- beta_z_y * z + rnorm(n)
dat <- data.frame(y = y, x = x, z = z)

x <- as.matrix(dat[, -1])

xg <- xgboost(data = x, label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)

# My SHAP
res <- myshap_rcpp(xg, x)

# Interaction TreeSHAP
uxg <- xgboost.unify(xg, x)
tsi <- treeshap(uxg, x, interactions = TRUE)

df <- rbind(data.frame(Variable = "x", x = x[, 1], y = res$m[, "x"], z = 0),
            data.frame(Variable = "z", x = x[, 2], y = res$m[, "z"], z = 0),
            data.frame(Variable = "x:z", x = x[, 1], y = x[, 2], z = res$m[, "x:z"]))
ggplot(df, aes(x = x, y = y, col = z)) +
  facet_wrap(~ Variable) +
  geom_point() +
  scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()
