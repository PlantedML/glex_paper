library(data.table)
library(ggplot2)
library(parallel)
source("rpf.R")
source("shap_rpf.R")
source("predict_rpf.R")

set.seed(42)

ntrees = 50
splits = 30

data(adult, package = "fairml")
adult[,"income"]<-as.numeric(factor(adult[,"income"]))-1   #### <50k =0, >50k=1

y <- as.numeric(adult$income)


x<- data.frame(adult)
x <- subset( x, select = -income )
x <- x[, c(1:4,9, 12)]


n <- nrow(x)
idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])


temp <- sapply(1:length(levels(train$x [,"workclass"])), function(k) mean(train$y[train$x [,"workclass"]==levels(adult[,"workclass"])[k]]))
train$x[,"workclass"] <- factor(train$x[,"workclass"], levels=levels(train$x[,"workclass"])[order(temp)])
test$x[,"workclass"] <- factor(test$x[,"workclass"], levels=levels(train$x[,"workclass"])[order(temp)])
sapply(1:length(levels(train$x[,"workclass"])), function(k) mean(train$y[train$x[,"workclass"]==levels(train$x[,"workclass"])[k]]))


temp  <- sapply(1:length(levels(train$x[,"education"])), function(k) mean(train$y[train$x[,"education"]==levels(train$x[,"education"])[k]]))
train$x[,"education"] <- factor(train$x[,"education"], levels=levels(train$x[,"education"])[order(temp)])
test$x[,"education"] <- factor(test$x[,"education"], levels=levels(train$x[,"education"])[order(temp)])
sapply(1:length(levels(train$x[,"education"])), function(k) mean(train$y[train$x[,"education"]==levels(train$x[,"education"])[k]]))



train$x <- as.data.table(lapply(train$x, as.numeric))
# Protected attribute (sex) 0=female, 1=male
train$x[, protected := sex - 1]
train$x[, sex := NULL]


test$x <- as.data.table(lapply(test$x, as.numeric))
# Protected attribute (sex) 0=female, 1=male
test$x[, protected := sex - 1]
test$x[, sex := NULL]


X=as.matrix(train$x)
Y=as.matrix(train$y)


#results from rpf
res_rpf <- rpf(X=X, Y=Y, max_interaction = 4, ntrees = ntrees, splits = splits, cores = 8)

#obtain shap decomposition
values <- individuals <- list()


res_shap <- shap_rpf(res_rpf, X, cores=8)



X_test=as.matrix(test$x)
res = pred_shap_rpf(X_test,res_shap, shap=TRUE, cores=8)



pred_full <- res[,1]
mean_pred <- mean(pred_full)

# Re-fit without sex

train$x<- as.matrix(train$x)
test$x<- as.matrix(test$x)


X_refit=train$x[,colnames(train$x)[colnames(train$x) != "protected"], drop = FALSE]
Y_refit=train$y

X_refit
Y_refit

rpf_refit <- rpf(X=X_refit, Y=Y_refit, max_interaction = 4, ntrees = ntrees, splits = splits)


X_refit_test <- test$x[, colnames(test$x) != "protected", drop = FALSE]


pred_refit <- predict_rpf(X_refit_test, rpf_refit)

# Remove sex from decomposed model
m_reduced <- res[, grep("protected", colnames(res), invert = TRUE), drop = FALSE]
pred_reduced <- rowSums(m_reduced[,2:(dim(m_reduced)[2]-6)])

# Plot
female_idx <- test$x[, "protected"] == 0
male_idx <- test$x[, "protected"] == 1
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
  geom_boxplot(outlier.shape = NA) +
  theme_bw()

# Save results
saveRDS(df, "res_adult.Rds")

# Median differences
medians <- df[, median(Prediction), by = .(Sex, Model)]
medians[, diff(V1), by = .(Model)]
