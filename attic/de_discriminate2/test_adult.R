
library(data.table)
library(xgboost)
library(shapdecomp)
library(ggplot2)

set.seed(42)

data(adult, package = "fairml")
dt <- as.data.table(lapply(adult, as.numeric))

# Protected attribute (sex) 0=female, 1=male
dt[, protected := sex - 1]
dt[, sex := NULL]

# Outcome
y <- dt$income - 1
dt[, income := NULL]

# Predictors
x <- as.matrix(dt)

# Temp
#x <- x[, c(1:4, 12, 13)]

# Train/test split
n <- nrow(x)
idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

xg <- xgboost(data = train$x, label = train$y, params = list(max.depth = 5, eta = .1), nrounds = 20)

# Compute decomposition
res <- shapdecomp(xg, test$x)

# Full model decomposition is same as prediction
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)

# Re-fit without protected attribute
xg_refit <- xgboost(data = train$x[, colnames(train$x) != "protected", drop = FALSE], label = train$y,
                    params = list(max.depth = 5, eta = .1), nrounds = 20, verbose = FALSE)
pred_refit <- predict(xg_refit, test$x[, colnames(test$x) != "protected", drop = FALSE])

# Remove protected attribute from decomposed model
#m_reduced <- res$m[, colnames(res$m) != "protected"]
m_reduced <- res$m[, grep("protected", colnames(res$m), invert = TRUE), drop = FALSE]
pred_reduced <- rowSums(m_reduced) + mean_pred

female_idx <- test$x[, "protected"] == 0
male_idx <- test$x[, "protected"] == 1
df <- rbind(data.table(Sex = "Female", Model = "Full", Prediction = pred_full[female_idx]),
            data.table(Sex = "Male", Model = "Full", Prediction = pred_full[male_idx]),
            data.table(Sex = "Female", Model = "Refitted", Prediction = pred_refit[female_idx]),
            data.table(Sex = "Male", Model = "Refitted", Prediction = pred_refit[male_idx]),
            data.table(Sex = "Female", Model = "Reduced", Prediction = pred_reduced[female_idx]),
            data.table(Sex = "Male", Model = "Reduced", Prediction = pred_reduced[male_idx]))
df[, Model_Sex := paste(Model, Sex, sep = "_")]

ggplot(df, aes(x = Sex, y = Prediction, fill = Model)) +
  geom_boxplot() +
  theme_bw()

ggsave("adult.pdf")
