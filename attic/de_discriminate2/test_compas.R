
library(data.table)
library(xgboost)
library(shapdecomp)
library(ggplot2)

data(compas, package = "fairml")
dt <- data.table(compas)
dt[, race := as.numeric(race)]

# Protected attribute (sex)
dt[, protected  := 1*(sex  == "Female")]
dt[, sex := NULL]

# Protected attribute (race)
# dt[, protected2  := race]
# dt[, race := NULL]

# Outcome
y <- 1*(dt$two_year_recid  == "Yes")
dt[, two_year_recid := NULL]

# Predictors
x <- as.matrix(dt)

# Temp
x <- x[, c(1:4, 14, 15)]

# Train/test split
n <- nrow(x)
idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

xg <- xgboost(data = train$x, label = train$y, params = list(max.depth = 5, eta = .5), nrounds = 10)

# Compute decomposition
res <- shapdecomp(xg, test$x)

# Full model decomposition is same as prediction
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)

# Re-fit without protected attribute
xg_refit <- xgboost(data = train$x[, colnames(train$x) != "protected"], label = train$y,
                    params = list(max.depth = 5, eta = .5), nrounds = 10, verbose = FALSE)
pred_refit <- predict(xg_refit, test$x[, colnames(test$x) != "protected"])

# Remove protected attribute from decomposed model
#m_reduced <- res$m[, colnames(res$m) != "protected"]
m_reduced <- res$m[, grep("protected", colnames(res$m), invert = TRUE), drop = FALSE]
pred_reduced <- rowSums(m_reduced) + mean_pred


plot(pred_full, pred_reduced, col = test$x[, "protected"]+1)
abline(0,1)

plot(pred_refit, pred_reduced, col = test$x[, "protected"]+1)
abline(0,1)

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



