
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)

compas <- fread("compas.csv", select = c(2:8), stringsAsFactors = TRUE)

# Convert to numeric
compas[, Two_yr_Recidivism := 1*(Two_yr_Recidivism == "yes")]
compas[, Age_Above_FourtyFive  := 1*(Age_Above_FourtyFive  == "yes")]
compas[, Age_Below_TwentyFive  := 1*(Age_Below_TwentyFive  == "yes")]
compas[, Female  := 1*(Female  == "Female")]
compas[, Misdemeanor  := 1*(Misdemeanor  == "yes")]
compas[, ethnicity := as.numeric(ethnicity)]

# Prepare data for xgboost
x <- as.matrix(compas[, -1])
y <- compas$Two_yr_Recidivism

# Sample from marginals and let unsupervised RF decide if out of support
x_synth <- as.data.table(lapply(compas[, -1], function(x) {
  sample(x, length(x), replace = TRUE)
}))
dat <- rbind(data.frame(y = 0, compas[, -1]),
             data.frame(y = 1, x_synth))
rf <- ranger(y ~ ., dat, classification = TRUE, probability = TRUE)
pred <- predict(rf, x_synth)$predictions[, 2]

# Start with likely out of support
x_new <- as.matrix(x_synth[order(pred, decreasing = TRUE), ])

# In each step, add data instance out of support with label to shrink SHAP values
x_modified <- x
y_modified <- y
iters_no_change <- 0

xg_nmod <- xgboost(data = x, label = y, params = list(max_depth = 5, eta = .05), nrounds = 10)
urf <- xgboost.unify(xg_nmod, x)
ts <- treeshap(urf, x, verbose = FALSE)
best_value <- mean(abs(ts$shaps[, "Female"]))

for (i in 1:nrow(x_new)) {
  message(i, " out of ", nrow(x_new))

  if (iters_no_change > 1000) {
    break
  }

  x_temp <- rbind(x_modified, x_new[i, ])

  # Try with 0
  y_temp <- c(y_modified, 0)
  xg <- xgboost(data = x_temp, label = y_temp, params = list(max_depth = 5, eta = .05), nrounds = 10, verbose = FALSE)
  urf <- xgboost.unify(xg, x_temp)
  ts <- treeshap(urf, x_temp, verbose = FALSE)
  new_value0 <- mean(abs(ts$shaps[, "Female"]))

  # Try with 1
  y_temp <- c(y_modified, 1)
  xg <- xgboost(data = x_temp, label = y_temp, params = list(max_depth = 5, eta = .05), nrounds = 10, verbose = FALSE)
  urf <- xgboost.unify(xg, x_temp)
  ts <- treeshap(urf, x_temp, verbose = FALSE)
  new_value1 <- mean(abs(ts$shaps[, "Female"]))

  message("new value ", min(new_value0, new_value1))

  if (best_value <= min(new_value0, new_value1)) {
    iters_no_change <- iters_no_change + 1
  } else {
    iters_no_change <- 0
    if (new_value0 <= new_value1) {
      best_value <- new_value0
      x_modified <- x_temp
      y_modified <- c(y_modified, 0)
    } else {
      best_value <- new_value1
      x_modified <- x_temp
      y_modified <- c(y_modified, 1)
    }
  }
}

# Fit on modified data
xg_mod <- xgboost(data = x_modified, label = y_modified, params = list(max_depth = 5, eta = .05), nrounds = 10)


# My SHAP
source("rcpp_implementation.R")
res_nmod <- myshap_rcpp(xg_nmod, x)
res_mod <- myshap_rcpp(xg_mod, x)

# Plot difference
nmod <- data.frame(modified = "not modified",
                 Female = factor(x[, "Female"]),
                 shap = res_nmod$shap[, "Female"],
                 m = res_nmod$m[, "Female"])
mod <- data.frame(modified = "modified",
                 Female = factor(x[, "Female"]),
                 shap = res_mod$shap[, "Female"],
                 m = res_mod$m[, "Female"])
df <- rbind(nmod, mod)

ggplot(df, aes(x = Female, y = shap)) +
  facet_wrap(~modified) +
  geom_boxplot() +
  theme_bw()

unique(res_nmod$m[, "Female"])

plot(x[, "Female"], res_nmod$shap[, "Female"])
