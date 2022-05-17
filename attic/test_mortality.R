

library(data.table)
library(ranger)
library(xgboost)
library(treeshap)
library(ggplot2)

source("rcpp_implementation.R")

# Load data
x_train <- fread("../treeexplainer-study/notebooks/mortality/X_strain.csv", drop = 1)[, 1:10]
x_mat <- as.matrix(x_train)
y_train <- fread("../treeexplainer-study/notebooks/mortality/y_strain.csv")$V1

# xgboost
dtrain <- xgb.DMatrix(x_mat, label = y_train)
params <- list(learning_rate = 0.001,
               max_depth = 4,
               subsample = 0.5,
               reg_lambda = 5.5,
               reg_alpha = 0,
               colsample_bytree = 1
)
#fit <- xgb.train(params = params, data = dtrain, nrounds = 6765)
fit <- xgb.train(params = params, data = dtrain, nrounds = 10)

# Treehap
urf <- xgboost.unify(fit, x_mat)
ts <- treeshap(urf, x_mat, verbose = FALSE)

# Interaction Treeshap
tsi <- treeshap(urf, x_mat, interactions = TRUE)

# Plot interactions
df_ts <- data.frame(age = x_train[, age],
                    sex = factor(x_train[, sex_isFemale], levels = c("TRUE", "FALSE"), labels = c("Female", "Male")),
                    interaction = tsi$interactions[1, 2, ] + tsi$interactions[2, 1, ])

p1 <- ggplot(df_ts, aes(x = age, y = interaction, col = sex)) +
  geom_point() +
  scale_colour_manual(values = c("red", "blue")) +
  theme_bw() +
  ggtitle("TreeSHAP")


# My SHAP
res <- myshap_rcpp(fit, x_mat)

df_my <- data.frame(age = x_train[, age],
                    sex = factor(x_train[, sex_isFemale], levels = c("TRUE", "FALSE"), labels = c("Female", "Male")),
                    interaction = res$m[, "sex_isFemale:age"])
p2 <- ggplot(df_my, aes(x = age, y = interaction, col = sex)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("red", "blue")) +
  theme_bw() +
  ggtitle("Our method")

require(gridExtra)
grid.arrange(p1, p2, ncol=2)

df <- data.frame(age = x_train[, age],
                 sex = factor(x_train[, sex_isFemale], levels = c("TRUE", "FALSE"), labels = c("Female", "Male")),
                 my_interaction = res$m[, "sex_isFemale:age"],
                 ts_interaction = tsi$interactions[1, 2, ] + tsi$interactions[2, 1, ])

ggplot(df, aes(x = age, y = ts_interaction, col = sex)) +
  geom_point() +
  geom_line(aes(y = my_interaction)) +
  scale_colour_manual(values = c("red", "blue")) +
  theme_bw()
