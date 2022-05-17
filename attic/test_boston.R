
library(data.table)
library(mlbench)
library(xgboost)
library(treeshap)
source("rcpp_implementation.R")

data("BostonHousing")

dat <- as.data.table(BostonHousing)
dat[, chas := as.numeric(chas) - 1]

x <- as.matrix(dat[, -14])
y <- as.matrix(dat[, 14])

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .5), nrounds = 10)

interactions <- myshap_rcpp(xg, x[1:10, ])

# SHAP values are the sum of the m's *1/d
shap <- sapply(colnames(x), function(col) {
  idx <- grep(col, colnames(interactions))
  rowSums(interactions[, idx])
})

shap
interactions

# treeshap for comparison
uxg <- xgboost.unify(xg, x[1:10, , drop = FALSE])
treeshap(uxg, x[1:10, , drop = FALSE])

# Interaction shap
treeshap(uxg, x[1:10, , drop = FALSE], interactions = TRUE)
