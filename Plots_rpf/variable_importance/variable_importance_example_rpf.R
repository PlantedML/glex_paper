library(mvtnorm)
library(ISLR2)
library(xgboost)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("rpf.R")
source("shap_rpf.R")

set.seed(2022)

# Function for variable importance
vim <- function(res, vars, order = NULL) {
  sapply(vars, function(colname) {
    # Select all columns including variable
    cols <-grep(colname, colnames(res))
    
    m <- res[, cols]
    
    # Interaction order
    d <- lengths(regmatches(colnames(m), gregexpr(",", colnames(m))))/2 + 1
    
    if (!is.null(order)) {
      m <- m[, d %in% order, drop = FALSE]
      d <- d[d %in% order, drop = FALSE]
    }
    
    # Variable importance
    if (ncol(m) == 1) {
      mean(abs(m) / d)
    } else {
      mean(rowSums(abs(m) %*% diag(1/d)))
    }
  })
}

ntrees = 50
splits = 30

# Simulated data ----------------------------------------------------------
# Simulated
n <- 10000
p <- 4
beta <- c(1, 0, 1, 0)
beta0 <- 0
cov_base <- 0
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 + 1 * x[, 2] * x[, 3] - 2 * x[, 2] * x[, 3] * x[, 4]
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

#results from rpf
res_rpf <- rpf(X=x, Y=y, max_interaction = 4, ntrees = ntrees, splits = splits)

#obtain shap decomposition
X<-as.matrix(x)

res_shap <- shap_rpf(res_rpf, X)

# results from the decomposition
res = pred_shap_rpf(x,res_shap, shap=TRUE)

# SHAP variable importance
vim_shap <- colMeans(abs(res[,(ncol(res)-3):ncol(res)]))

# Plot
df <- data.table(melt(as.data.table(vim_shap)))
colnames(df) <- c("Variable","VIM")
df[,variable := colnames(x)]
p_sim_shap <- ggplot(df, aes(x = Variable, y = VIM)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d()+
  theme_bw()

# Variable importance for all orders separately
vim_sep <- sapply(1:4, vim, res = res, vars = colnames(x))

vim_table <- expand.grid(rownames(vim_sep),c(1,2,3,4),0)

for(v1 in 1:nrow(vim_table)){
  
  vim_table[v1,3]=vim_sep[vim_table[v1,1], vim_table[v1,2]]
}

# Plot
df <- as.data.table(vim_table)
colnames(df) <- c("Variable", "Order", "VIM")
df[, Order := factor(Order, levels = 1:4,
                     labels = c("Main effect", paste0(2:4, "-way interaction")))]
p_sim_sep <- ggplot(df, aes(x = Variable, y = VIM, fill = Order)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d()+
  theme_bw()

# Bike data ---------------------------------------------------------------
# Prepare data
data(Bikeshare)
bike <- data.table(Bikeshare)
bike[, hr := as.numeric(as.character(hr))]
x <- as.matrix(bike[, .(day, hr, temp, windspeed, workingday, hum)])
y <- bike$bikers

#results from rpf
res_rpf <- rpf(X=x, Y=y, max_interaction = 4, ntrees = ntrees, splits = splits)

#obtain shap decomposition
X<-as.matrix(x)

res_shap <- shap_rpf(res_rpf, X)

# results from the decomposition
res = pred_shap_rpf(x,res_shap, shap=TRUE)

# SHAP variable importance
vim_shap <- colMeans(abs(res[,((ncol(res)-5):(ncol(res)))[-4]]))

# Plot
df <- data.table(melt(as.data.table(vim_shap)))
colnames(df) <- c("Variable","VIM")
df[,variable := colnames(x)[-4]]
p_bike_shap <- ggplot(df, aes(x = Variable, y = VIM)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(alpha = 0.9)+
  theme_bw()

# Variable importance for all orders separately
vim_sep <- sapply(1:4, vim, res = res, vars = colnames(x)[-4])

vim_table <- expand.grid(rownames(vim_sep),c(1,2,3,4),0)

for(v1 in 1:nrow(vim_table)){
  
  vim_table[v1,3]=vim_sep[vim_table[v1,1], vim_table[v1,2]]
}

# Plot
df <- as.data.table(vim_table)
colnames(df) <- c("Variable", "Order", "VIM")
df[, Order := factor(Order, levels = 1:4,
                     labels = c("Main effect", paste0(2:4, "-way interaction")))]
p_bike_sep <- ggplot(df, aes(x = Variable, y = VIM, fill = Order)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(alpha = 0.9)+
  theme_bw()

# Plot all together -------------------------------------------------------
plot_grid(p_sim_shap, p_sim_sep, p_bike_shap, p_bike_sep, ncol = 2, rel_widths = c(.4, .6))

ggsave("variable_importance_rpf.png", width = 9, height = 4)