
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
xg <- xgboost(data = x, label = y, params = list(max_depth = 2, eta = .5), nrounds = 1)
trees <- xgb.model.dt.tree(model = xg, use_int_id = TRUE)

# Function to get all subsets of set
subsets <- function(x) {
  do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
}

# Recursively go through trees only once and calculate expectation for all possible subsets

# This calculates the expectation of a tree when the given features are in the coalition
# Fills a matrix with obs in rows and subsets U in columns
tree <- 0
T <- trees[Tree == tree & Feature != "Leaf", unique(Feature)]
U <- subsets(T)
U_char <- sapply(U, paste, collapse = "")
#mat <- matrix(0, nrow = length(obs), ncol = length(U))

all_obs <- 1:4

recurse <- function(tree, node_id, subset_idx, obs) {
  #message("start node ", node_id)
  mat <- matrix(0, nrow = length(all_obs), ncol = length(U))

  # if (length(obs) == 0) {
  #   mat
  # }

  # if (node_id == 11) {
  #   browser()
  # }

  if (trees[Tree == tree & Node == node_id, Feature == "Leaf"]) {
    # For leaf, return prediction for all subsets and obs
    #message("x")
    mat[obs, subset_idx] <- trees[Tree == tree & Node == node_id, Quality]
    #message("y")
  } else {
    feat <- trees[Tree == tree & Node == node_id, Feature]
    split <- trees[Tree == tree & Node == node_id, Split]

    # Get obs going left and right
    idx_yes <- x[obs, feat] <= split
    id_yes <- trees[Tree == tree & Node == node_id, Yes]
    id_no <- trees[Tree == tree & Node == node_id, No]

    # Get subsets where splitting feature is in
    idx_in <- sapply(U, function(x) {feat %in% x})

    # TODO: What to do for empty subsets?
    # For subsets where feature is in, send left and right
    #message("AAA")
    #browser()
    if (any(idx_in)) {
      if (any(idx_yes)) {
        mat <- mat + recurse(tree, id_yes, idx_in, obs[idx_yes])
      }
      if (any(!idx_yes)) {
        mat <- mat + recurse(tree, id_no, idx_in, obs[!idx_yes])
      }
    }

    if (any(!idx_in)) {
      # For subsets where feature is out, weighted average
      cover_own <- trees[Tree == tree & Node == node_id, Cover]
      cover_yes <- trees[Tree == tree & Node == id_yes, Cover]
      cover_no <- trees[Tree == tree & Node == id_no, Cover]

      exp_yes <- recurse(tree, id_yes, !idx_in, obs)
      exp_no <- recurse(tree, id_no, !idx_in, obs)

      #browser()

      mat <- mat + cover_yes / cover_own * exp_yes + cover_no / cover_own * exp_no
    }

  }
  #message("end node ", node_id)
  mat
}

mat <- recurse(tree, 0, rep(TRUE, length(U)), all_obs)
colnames(mat) <- U_char


# Function to calculate m_S (model without feature subset S)
m_S <- function(S) {
  sapply(0:max(trees$Tree), function(tree) {
    if (trees[Tree == tree, all(S %in% Feature)]) {
      # For all subsets U in tree
      T <- trees[Tree == tree & Feature != "Leaf", unique(Feature)]
      rowSums(sapply(subsets(T), function(U) {
        if ((length(setdiff(T, S)) == 0) || all(setdiff(T, S) %in% U)) {
          # TODO: Check for depth>1
          colname <- paste(U, collapse = "")
          if (nchar(colname) == 0) {
            colname <- 1
          }
          message("U: ", paste(U, collapse = ""), ", res: ", (-1)^(length(S) - length(setdiff(T, U))) * mat[2, colname])
          (-1)^(length(S) - length(setdiff(T, U))) * mat[, colname]
        } else {
          rep(0, length(all_obs))
        }
      }))
    } else {
      rep(0, length(all_obs))
    }
  })
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
