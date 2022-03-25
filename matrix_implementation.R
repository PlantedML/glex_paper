
library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)

set.seed(44)

# Simulated
n <- 100
p <- 4
beta <- c(2, 1, .5, 0)
beta0 <- 0
cov_base <- 0
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .5), nrounds = 30)
trees <- xgb.model.dt.tree(model = xg, use_int_id = TRUE)

# Function to get all subsets of set
subsets <- function(x) {
  do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
}

# Recursively go through trees only once and calculate expectation for all possible subsets

# This calculates the expectation of a tree when the given features are in the coalition
# Fills a matrix with obs in rows and subsets U in columns
recurse <- function(tree, node, U) {
  # If leaf, just return value
  if (trees[Tree == tree & Node == node, Feature == "Leaf"]) {
    pred <- trees[Tree == tree & Node == node, Quality]
    mat <- matrix(pred, nrow = length(all_obs), ncol = length(U))
  } else {
    # Start with all 0
    mat <- matrix(0, nrow = length(all_obs), ncol = length(U))

    # Get left and right nodes
    node_yes <- trees[Tree == tree & Node == node, Yes]
    node_no <- trees[Tree == tree & Node == node, No]

    # Call both children, they give a matrix each of all obs and subsets
    mat_yes <- recurse(tree, node_yes, U)
    mat_no <- recurse(tree, node_no, U)

    # Get subsets where splitting feature is out
    feat <- trees[Tree == tree & Node == node, Feature]
    split <- trees[Tree == tree & Node == node, Split]
    subsets_out <- sapply(U, function(x) {feat %in% x})

    # For subsets where feature is in, split to left/right
    obs_idx_yes <- x[all_obs, feat] <= split
    node_yes <- trees[Tree == tree & Node == node, Yes]
    node_no <- trees[Tree == tree & Node == node, No]

    mat[obs_idx_yes, !subsets_out] <- mat[obs_idx_yes, !subsets_out, drop = FALSE] + mat_yes[obs_idx_yes, !subsets_out, drop = FALSE]
    mat[!obs_idx_yes, !subsets_out] <- mat[!obs_idx_yes, !subsets_out, drop = FALSE] + mat_no[!obs_idx_yes, !subsets_out, drop = FALSE]

    # For subsets where feature is out, weighted average of left/right
    cover_own <- trees[Tree == tree & Node == node, Cover]
    cover_yes <- trees[Tree == tree & Node == node_yes, Cover]
    cover_no <- trees[Tree == tree & Node == node_no, Cover]
    exp_yes <- mat_yes[, subsets_out, drop = FALSE]
    exp_no <- mat_no[, subsets_out, drop = FALSE]
    mat[, subsets_out] <- mat[, subsets_out, drop = FALSE] + cover_yes / cover_own * exp_yes + cover_no / cover_own * exp_no
  }

  # Return combined matrix
  mat
}

all_obs <- 1:4

mats <- lapply(0:max(trees$Tree), function(tree) {
  T <- trees[Tree == tree & Feature != "Leaf", unique(Feature)]
  U <- subsets(T)
  mat <- recurse(tree = tree, node = 0, U = U)
  colnames(mat) <- sapply(U, paste, collapse = "")
  mat
})


# Function to calculate m_S (model without feature subset S)
m_S <- function(S) {
  rowSums(sapply(0:max(trees$Tree), function(tree) {
    if (trees[Tree == tree, all(S %in% Feature)]) {
      # For all subsets U in tree
      T <- trees[Tree == tree & Feature != "Leaf", unique(Feature)]
      rowSums(sapply(subsets(T), function(U) {
        if ((length(setdiff(T, S)) == 0) || all(setdiff(T, S) %in% U)) {
          colname <- paste(U, collapse = "")
          if (nchar(colname) == 0) {
            colname <- 1
          }
          (-1)^(length(S) - length(setdiff(T, U))) * mats[[tree+1]][, colname]
        } else {
          rep(0, length(all_obs))
        }
      }))
    } else {
      rep(0, length(all_obs))
    }
  }))
}

# Calculate m_S for all subsets S
all_S <- subsets(colnames(x))
m_all <- sapply(all_S, m_S)
colnames(m_all) <- sapply(all_S, paste, collapse = "")

# SHAP values are the sum of the m's *1/d
d <- sapply(colnames(m_all), nchar)/2
shap <- sapply(colnames(x), function(col) {
  idx <- grep(col, colnames(m_all))
  m_all[, idx] %*% (1/d[idx])
})
shap


# treeshap for comparison
uxg <- xgboost.unify(xg, x[all_obs, , drop = FALSE])
treeshap(uxg, x[all_obs, , drop = FALSE])

# Our interactions
m_all[, -1] / d[-1]

# Interaction shap
treeshap(uxg, x[all_obs, , drop = FALSE], interactions = TRUE)
