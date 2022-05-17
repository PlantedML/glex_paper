library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(gridExtra)
library(shapdecomp)

#set.seed(2022)

# Simulated
n <- 10000
p <- 4
beta <- c(1, 0, 1, 0)
beta0 <- 0
cov_base <- 0.3
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 - 1 * x[, 2] * x[, 3]
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# Train/test split
n <- nrow(x)
idx <- sample(n, 2/3*n)
train <- list(x = x[idx, ], y = y[idx])
test <- list(x = x[-idx, ], y = y[-idx])

# xgboost
xg <- xgboost(data = train$x, label = train$y, params = list(max_depth = 4, eta = .1), nrounds = 20)

# SHAP decomposition
res <- shapdecomp(xg, test$x)

# SHAP variable importance
vim_shap <- colMeans(abs(res$shap))

# Function for variable importance
vim <- function(res, vars, order = NULL) {
  sapply(vars, function(colname) {
    # Select all columns including variable
    cols <- c(
      grep(paste0("^", colname, "$"), colnames(res$m)),
      grep(paste0("^", colname, ":"), colnames(res$m)),
      grep(paste0(":", colname, "$"), colnames(res$m)),
      grep(paste0(":", colname, ":"), colnames(res$m))
    )
    m <- res$m[, cols]

    # Interaction order
    d <- lengths(regmatches(colnames(m), gregexpr(":", colnames(m)))) + 1

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

# # Complete variable importance (including interactions)
# vim_all <- vim(res, colnames(x))
#
# # Interaction variable importance (all interactions)
# vim_int <- vim(res, colnames(x), order = 2:4)
#
# # Main effect variable importance
# vim_main <- colMeans(abs(res$m[, colnames(x)]))

# Variable importance for all orders separately
vim_sep <- sapply(1:4, vim, res = res, vars = colnames(x))

# Plot
df <- data.table(melt(vim_sep))
colnames(df) <- c("Variable", "Order", "VIM")
df[, Order := factor(Order, levels = 1:4,
                     labels = c("Main effect", paste0(2:4, "-way interaction")))]
ggplot(df, aes(x = Variable, y = VIM, fill = Order)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d()+
  theme_bw()

