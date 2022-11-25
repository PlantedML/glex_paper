
library(xgboost)
library(glex)
#library(shapdecomp)
library(treeshap)
library(microbenchmark)
library(foreach)
library(data.table)
library(ggplot2)

# Treeshap in one function
fun_ts <- function(xg, x_train, x_test, ...) {
  unified_model <- xgboost.unify(xg, as.matrix(x_train))
  treeshap(unified_model, x_test, ...)
}

# Data simulation function
sim_dat <- function(n, p) {
  beta <- c(rep(1, 2), rep(0, p-2))
  beta0 <- 0
  x <- matrix(rnorm(n = 2 * n * p), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))
  lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
  y <- lp + rnorm(n)

  x_train <- x[1:n, ]
  x_test <- x[(n+1):(2*n), ]
  y_train <- y[1:(n)]
  y_test <- y[(n+1):(2*n)]

  list(x_train = x_train, y_train = y_train,
       x_test = x_test, y_test = y_test)
}

# Compare function
compare_fun <- function(n, p, b, depth, rounds) {
  dat <- sim_dat(n, p)
  xg <- xgboost(data = dat$x_train, label = dat$y_train, verbose = FALSE,
                params = list(max_depth = depth, eta = .1), nrounds = rounds)

  # Compare runtime
  res <- microbenchmark(
    glex2 = glex(xg, dat$x_test, max_interaction = 2),
    glex = glex(xg, dat$x_test, max_interaction = Inf),
    treeshap = fun_ts(xg, dat$x_train, dat$x_test, interactions = TRUE, verbose = FALSE),
    xgboost = predict(xg, dat$x_test, predinteraction = TRUE),
    times = 1
  )
  data.table(repl = b, n = n, p = p, depth = depth, rounds = rounds, algo = res$expr, time = res$time)
}

# Run benchmark
df <- foreach(bb = 1:5, .combine = rbind) %:%
  foreach(dd = round(seq(2, 10, length.out = 10)), .combine = rbind) %dopar%
  compare_fun(b = bb, n = 1000, p = 6, depth = dd, rounds = 100)

# Save
saveRDS(df, "runtime_depth.Rds")

# Plot
df[, seconds := time/1e9]
df2 <- df[, .(seconds = median(seconds)), by = .(depth, algo)]
ggplot(df2, aes(x = depth, y = seconds, col = algo)) +
  geom_point() +
  geom_line() +
  theme_bw()





