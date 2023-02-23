library(data.table)
library(ggplot2)

source("rpf.R")
source("shap_rpf.R")
source("predict_rpf.R")

set.seed(42)

n <- 10000

ntrees = 50
splits = 30

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

X=train$x
Y=train$y

X
Y
#results from rpf
res_rpf <- rpf(X=X, Y=Y, max_interaction = 4, ntrees = ntrees, splits = splits, cores=8)

#obtain shap decomposition
X<-as.matrix(x)

res_shap <- shap_rpf(res_rpf, X, cores=8)

X_test=test$x

X_test

res = pred_shap_rpf(X_test,res_shap, shap=TRUE, cores=8)

pred_full <- res[,1]
mean_pred <- mean(pred_full)

# Re-fit without sex

X_refit=train$x[,colnames(train$x) != "sex", drop = FALSE]

Y_refit=train$y

X_refit
Y_refit

rpf_refit <- rpf(X=X_refit, Y=Y_refit, max_interaction = 1, ntrees = ntrees, splits = splits, cores=8)

pred_refit <- predict_rpf(test$x[, colnames(test$x) != "sex", drop = FALSE], rpf_refit)

# Remove sex from decomposed model
m_reduced <- res[, grep("sex", colnames(res), invert = TRUE), drop = FALSE]
pred_reduced <- rowSums(m_reduced[,2:3])

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
