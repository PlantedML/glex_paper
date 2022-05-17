library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(shapdecomp)

set.seed(42)

n <- 10000

# Simulation
sex <- rbinom(n, 1, .5) # 0=female, 1=male
hours <- 30 + 10 * sex + 1*rnorm(n) # women work 30 hours, men 40
y <- 20 * sex + 1 * hours + rnorm(n) # men get 20k extra
dat <- data.frame(y = y, sex = sex, hours = hours)
x <- as.matrix(dat[, -1])

# Train/test
idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

# Fit model
xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 4, eta = .1), nrounds = 50)
pred_full <- predict(xg, test$x)
mean_pred <- mean(pred_full)

# Compute decomposition
res <- shapdecomp(xg, test$x)

# Re-fit without sex
xg_refit <- xgboost(data = train$x[, colnames(train$x) != "sex", drop = FALSE], label = train$y,
                    params = list(max.depth = 4, eta = .1), nrounds = 50, verbose = FALSE)
pred_refit <- predict(xg_refit, test$x[, colnames(test$x) != "sex", drop = FALSE])

# Remove sex from decomposed model
m_reduced <- res$m[, grep("sex", colnames(res$m), invert = TRUE), drop = FALSE]
pred_reduced <- rowSums(m_reduced) + mean_pred

# Plot
female_idx <- test$x[, "sex"] == 0
male_idx <- test$x[, "sex"] == 1
df <- rbind(data.table(Sex = "Female", Model = "Full", Prediction = pred_full[female_idx]),
            data.table(Sex = "Male", Model = "Full", Prediction = pred_full[male_idx]),
            data.table(Sex = "Female", Model = "Refitted", Prediction = pred_refit[female_idx]),
            data.table(Sex = "Male", Model = "Refitted", Prediction = pred_refit[male_idx]),
            data.table(Sex = "Female", Model = "Reduced", Prediction = pred_reduced[female_idx]),
            data.table(Sex = "Male", Model = "Reduced", Prediction = pred_reduced[male_idx]))
df[, Model_Sex := paste(Model, Sex, sep = "_")]
df[, Model := factor(Model, levels = c("Full", "Refitted", "Reduced"),
                     labels = c("Full", "Refitted", "Decomposed"))]

ggplot(df, aes(x = Sex, y = Prediction, fill = Model)) +
  geom_boxplot() +
  theme_bw()

# Save results
saveRDS(df, "res_sim.Rds")

# Median differences
medians <- df[, median(Prediction), by = .(Sex, Model)]
medians[, diff(V1), by = .(Model)]
