
library(mvtnorm)
library(ISLR2)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(shapdecomp)

set.seed(2022)

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

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 20)

# SHAP decomposition
res <- shapdecomp(xg, x)

# SHAP variable importance
vim_shap <- colMeans(abs(res$shap))

# Plot
df <- data.table(melt(vim_shap))
colnames(df) <- c("VIM")
df[, Variable := colnames(x)]
p_sim_shap <- ggplot(df, aes(x = Variable, y = VIM)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(alpha = .9) +
  theme_bw() +
  ylab("Variable importance")

# Variable importance for all orders separately
vim_sep <- sapply(1:4, vim, res = res, vars = colnames(x))

# Plot
df <- data.table(melt(vim_sep))
colnames(df) <- c("Variable", "Order", "VIM")
df[, Order := factor(Order, levels = 1:4,
                     labels = c("Main effect", paste0(2:4, "-way interaction")))]
p_sim_sep <- ggplot(df, aes(x = Variable, y = VIM, fill = Order)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(alpha = .9) +
  theme_bw() +
  ylab("Variable importance")

# Bike data ---------------------------------------------------------------
# Prepare data
data(Bikeshare)
bike <- data.table(Bikeshare)
bike[, hr := as.numeric(as.character(hr))]
x <- as.matrix(bike[, .(day, hr, temp, windspeed, workingday, hum)])
y <- bike$bikers

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 20)

# SHAP decomposition
res <- shapdecomp(xg, x)

# SHAP variable importance
vim_shap <- colMeans(abs(res$shap))

# Plot
df <- data.table(melt(vim_shap))
colnames(df) <- c("VIM")
df[, Variable := colnames(x)]
df <- df[Variable != "windspeed"]
p_bike_shap <- ggplot(df, aes(x = Variable, y = VIM)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(alpha = .9)+
  theme_bw() +
  ylab("Variable importance")

# Variable importance for all orders separately
vim_sep <- sapply(1:4, vim, res = res, vars = colnames(x))

# Plot
df <- data.table(melt(vim_sep))
colnames(df) <- c("Variable", "Order", "VIM")
df[, Order := factor(Order, levels = 1:4,
                     labels = c("Main effect", paste0(2:4, "-way interaction")))]
df <- df[Variable != "windspeed"]
p_bike_sep <- ggplot(df, aes(x = Variable, y = VIM, fill = Order)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(alpha = .9) +
  theme_bw() +
  ylab("Variable importance")

# Plot all together -------------------------------------------------------
prow <- plot_grid(p_sim_shap + theme(legend.position = "none"),
          p_sim_sep + theme(legend.position = "none", axis.title.y = element_blank()),
          p_bike_shap + theme(legend.position = "none"),
          p_bike_sep + theme(legend.position = "none", axis.title.y = element_blank()),
          ncol = 2, rel_widths = c(.513, .487))
legend <- get_legend(
  p_sim_sep + theme(legend.position = "bottom", legend.title = element_blank())
)
plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("variable_importance.pdf", width = 6, height = 4)
