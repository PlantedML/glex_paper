compas <- read.csv("compas.csv", sep="")

compas[,1] <- as.numeric(factor(compas[,1]))-1

#### convert data into numeric; delete column 8,9 which are pre-fitted response
mm<-compas[,-c(1,8,9)]
mm[,2:6] <- apply(mm[,2:6], 2,function(v){as.numeric(factor(v))} )



# pc <- prcomp(mm,
#              center = FALSE,
#              scale. = FALSE)
# print(pc)
# plot(pc$x[,1],pc$x[,2] )
# lines(0.299562005-1,  2.236427627+1, type="p", col="green")
# t(t(c(0.299562005-1,   2.236427627+1,  1.412962190,  0.295697586,  1.684119898, -0.194269313) %*% t(+65)) * pc$scale + pc$center)
#
#
#
# apply(mm,2,range)
# apply(mm,2,unique)




###### create secon data-set mm2 where ethnicity only has 2 different values
mm2 <- mm
mm2[mm2[,6]>1,] <- 2

########## NOW: create fake data set, new.data, that lives outside the support of the compas data.
new.data<-rep(NA,6)
alpha <- 5
start <- range(mm2[,1])[1]
end <- range(mm2[,1])[2]

for(i in 1:2){
  for (j in (1:2)){
    for (k in (1:2)){
      for (l in (1:2)){
        for (m in (1:2)){

          subdata <- mm2[apply(mm2[,2:6],1, function(z) all(z==c(i,j,k,l,m))),1]
          if (length(subdata)==0)  next

          my.range <- range(subdata)

          if (my.range[1]!=Inf)   {
          lower.band <-  my.range[1] - start
          if(lower.band>0){
          new.entries <- runif(round(lower.band*alpha ), min=start, max=my.range[1])
        if(m==1) new.data.temp <- cbind(new.entries, i,j,k,l,m)
        if(m==2) new.data.temp <- cbind(new.entries, i,j,k,l,sample(2:6,1))

         new.data <- rbind(new.data, new.data.temp)
          }
          }
          if (my.range[2]!=-Inf)   {
            upper.band <-  end - my.range[2]
            if(upper.band>0){
            new.entries <- runif(round(upper.band*alpha ), min=my.range[2], max=end)
            if(m==1) new.data.temp <- cbind(new.entries, i,j,k,l,m)
            if(m==2) new.data.temp <- cbind(new.entries, i,j,k,l,sample(2:6,1))
            new.data <- rbind(new.data, new.data.temp)
            }
          }

        }
      }
    }
  }
}

new.data<- new.data[-1,]


#### generate random response
new.data<- cbind(sample(c(0,1), nrow(new.data), replace=TRUE), new.data)

# Find response values which shrink SHAP values towards zero
# Start with all 0 and see if switching to 1 reduces abs(SHAP)
library(treeshap)
new.data[, 1] <- 0
param <- list(max_depth =5, eta = 0.05, verbose = 2, nthread = 1, objective="binary:logistic")

x <- as.matrix(rbind(as.matrix(mm),as.matrix(new.data[,-1])))
label <- c(as.numeric(compas[,1]),new.data[,1])

dtrain <- xgb.DMatrix(x, label = label)
bst <- xgb.train(param, dtrain, nrounds = 100)
res <- myshap_rcpp(bst, as.matrix(mm))
#last_value <- mean(abs(res$shap[, "Female"]))

for (i in 1:nrow(new.data)) {
  message(i, " out of ", nrow(new.data))
  # Switch value
  label2 <- label
  if (label2[nrow(compas)+i] == 0) {
    label2[nrow(compas)+i] <- 1
  } else {
    label2[nrow(compas)+i] <- 0
  }
  dtrain <- xgb.DMatrix(x, label = label2)
  bst <- xgb.train(param, dtrain, nrounds = 100)
  #res <- myshap_rcpp(bst, as.matrix(mm))

  urf <- xgboost.unify(bst, as.matrix(mm))
  ts <- treeshap(urf, as.matrix(mm), verbose = FALSE)

  #aa <- predict(bst, as.matrix(mm), predcontrib = TRUE, approxcontrib = TRUE)

  new_value <- mean(abs(ts$shaps[, "Female"]))

  if (new_value < last_value) {
    label <- label2
    last_value <- new_value
  }
  message("value ", last_value)
}


##### train xgboost on  original data and on second data where new.data is attached to compas data

library(xgboost)

param <- list(max_depth =5, eta = 0.05, verbose = 2, nthread = 1, objective="binary:logistic")
#dtrain <- xgb.DMatrix( as.matrix(rbind(as.matrix(mm),as.matrix(new.data[,-1]))), label =c(as.numeric(compas[,1]),new.data[,1]))
dtrain <- xgb.DMatrix( as.matrix(rbind(as.matrix(mm),as.matrix(new.data[,-1]))), label = label)

bst <- xgb.train(param, dtrain, nrounds = 100) # 2000

res <- predict(bst, as.matrix(mm))


param <- list(max_depth =5, eta = 0.05, verbose = 2, nthread = 1, objective="binary:logistic")
dtrain1 <- xgb.DMatrix( as.matrix(rbind(as.matrix(mm))), label =c(as.numeric(compas[,1])))
bst1 <- xgb.train(param, dtrain1, nrounds = 100) # 1000

res1 <- predict(bst1, as.matrix(mm))





cor(as.numeric(compas[,1]),res1)
cor(as.numeric(compas[,1]),res)
cor(res,res1)
plot(res,res1)
#
# range(mm[apply(mm[,2:6],1, function(i) all(i==c(2,2,2,2,6))),1])
# range(mm[apply(mm[,2:6],1, function(i) all(i==c(2,1,1,1,6))),1])
# range(mm[apply(mm[,2:6],1, function(i) all(i==c(1,2,2,2,6))),1])
# range(mm[apply(mm[,2:6],1, function(i) all(i==c(1,1,2,2,6))),1])
# range(mm[apply(mm[,2:6],1, function(i) all(i==c(1,1,2,2,6))),1])
#
# range(mm[,1])


# My SHAP
source("rcpp_implementation.R")
res <- myshap_rcpp(bst, as.matrix(mm))
res1 <- myshap_rcpp(bst1, as.matrix(mm))

library(ggplot2)
df <- melt(res$shap)
ggplot(df, aes(x = Var2, y = value)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  theme_bw() +
  coord_flip()

df1 <- melt(res1$shap)
ggplot(df1, aes(x = Var2, y = value)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  theme_bw() +
  coord_flip()

# ethnicity
aa <- data.frame(modified = "modified",
                 ethnicity = factor(mm$ethnicity),
                 shap = res$shap[, "ethnicity"],
                 m = res$m[, "ethnicity"])
bb <- data.frame(modified = "not modified",
                 ethnicity = factor(mm$ethnicity),
                 shap = res1$shap[, "ethnicity"],
                 m = res1$m[, "ethnicity"])
cc <- rbind(aa, bb)

ggplot(cc, aes(x = ethnicity, y = shap)) +
  facet_wrap(~modified) +
  geom_boxplot() +
  theme_bw()

# Female
aa <- data.frame(modified = "modified",
                 Female = factor(mm$Female),
                 shap = res$shap[, "Female"],
                 m = res$m[, "Female"])
bb <- data.frame(modified = "not modified",
                 Female = factor(mm$Female),
                 shap = res1$shap[, "Female"],
                 m = res1$m[, "Female"])
cc <- rbind(aa, bb)

ggplot(cc, aes(x = Female, y = shap)) +
  facet_wrap(~modified) +
  geom_boxplot() +
  theme_bw()
