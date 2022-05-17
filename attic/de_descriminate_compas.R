
library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)

source("rcpp_implementation.R")

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

n <- nrow(x)
idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)

# My SHAP
res <- myshap_rcpp(xg, test$x)

# Interaction TreeSHAP
#uxg <- xgboost.unify(xg, x)
#tsi <- treeshap(uxg, x, interactions = TRUE)

# Plot SHAP
df <- rbind(data.frame(Variable = "Number_of_Priors", x = test$x[, "Number_of_Priors"], y = res$shap[, "Number_of_Priors"]),
            data.frame(Variable = "Female", x = test$x[, "Female"], y = res$shap[, "Female"]),
            data.frame(Variable = "ethnicity", x = test$x[, "ethnicity"], y = res$shap[, "ethnicity"]))
ggplot(df, aes(x = x, y = y)) +
  facet_wrap(~ Variable) +
  geom_point() +
  #scale_color_gradient2(low = "blue", high = "red", mid = "grey") +
  theme_bw()

# Plot m
df <- rbind(data.frame(Variable = "Number_of_Priors", x = test$x[, "Number_of_Priors"], y = res$m[, "Number_of_Priors"]),
            data.frame(Variable = "Female", x = test$x[, "Female"], y = res$m[, "Female"]),
            data.frame(Variable = "ethnicity", x = test$x[, "ethnicity"], y = res$m[, "ethnicity"]))
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

# Remove sex from model
m_nosex <- res$m[, grep("Female", colnames(res$m), invert = TRUE)]
m_reduced <- rowSums(m_nosex) + mean_pred
plot(m_reduced, predict(xg, test$x))

# Re-fit without sex
xg_nosex <- xgboost(data = train$x[, -4], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
pred_nosex <- predict(xg_nosex, test$x[, -4])

df <- rbind(data.frame(model = "original", sex = factor(test$x[, "Female"]), pred = pred_full),
            data.frame(model = "refit", sex = factor(test$x[, "Female"]), pred = pred_nosex),
            data.frame(model = "reduce", sex = factor(test$x[, "Female"]), pred = m_reduced))
ggplot(df, aes(x = model, y = pred, fill = sex)) +
  geom_boxplot() +
  theme_bw()

# Remove ethnicity from model
m_nosex <- res$m[, grep("ethnicity", colnames(res$m), invert = TRUE)]
m_reduced <- rowSums(m_nosex) + mean_pred
plot(m_reduced, predict(xg, test$x))

# Re-fit without sex
xg_nosex <- xgboost(data = train$x[, -6], label = train$y, params = list(max_depth = 5, eta = .5), nrounds = 10)
pred_nosex <- predict(xg_nosex, test$x[, -6])

df <- rbind(data.frame(model = "original", ethnicity = factor(test$x[, "ethnicity"]), pred = pred_full),
            data.frame(model = "refit", ethnicity = factor(test$x[, "ethnicity"]), pred = pred_nosex),
            data.frame(model = "reduce", ethnicity = factor(test$x[, "ethnicity"]), pred = m_reduced))
df$model <- factor(df$model, levels = c("original", "refit", "reduce"), labels = c("original", "refit", "reduce"))
ggplot(df, aes(x = model, y = pred, fill = ethnicity)) +
  geom_boxplot() +
  theme_bw()


# Equalized odds proposes that the predictor and the protected attribute should be independent, conditional on the outcome.
lm(test$x[, "x2"] ~ test$x[, "race"] + pred_full)
lm(test$x[, "x2"] ~ test$x[, "race"] + m_reduced)
lm(test$x[, "x2"] ~ test$x[, "race"] + pred_norace)

# Well-calibrated systems propose that the outcome and protected attribute are independent, conditional on the predictor.
lm(pred_full ~ test$x)
lm(m_reduced ~ test$x)
lm(pred_nosex ~ test$x)

