
library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)

#set.seed(44)

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
xg <- xgboost(data = x, label = y, params = list(max_depth = 3, eta = .5), nrounds = 1)
trees <- xgb.model.dt.tree(model = xg, use_int_id = TRUE)

# Function to get all subsets of set
subsets <- function(x) {
  do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
}

# Recursively go through trees
# This calculates the expectation of a tree when the given features are in the coalition
recurse <- function(tree, node_id, feats_in, obs) {
  #message("node id ", node_id)
  if (trees[Tree == tree & Node == node_id, Feature == "Leaf"]) {
    # For leaf, return prediction
    trees[Tree == tree & Node == node_id, Quality]
  } else {
    feat <- trees[Tree == tree & Node == node_id, Feature]
    split <- trees[Tree == tree & Node == node_id, Split]
    if (x[obs, feat] <= split) {
      next_id <-  trees[Tree == tree & Node == node_id, Yes]
    } else {
      next_id <-  trees[Tree == tree & Node == node_id, No]
    }

    if (feat %in% feats_in) {
      # If splitting variable is in, split
      recurse(tree, next_id, feats_in, obs)
    } else {
      # If not, weighted average
      yes <- trees[Tree == tree & Node == node_id, Yes]
      no <- trees[Tree == tree & Node == node_id, No]
      cover_own <- trees[Tree == tree & Node == node_id, Cover]
      cover_yes <- trees[Tree == tree & Node == yes, Cover]
      cover_no <- trees[Tree == tree & Node == no, Cover]

      exp_yes <- recurse(tree, yes, feats_in, obs)
      exp_no <- recurse(tree, no, feats_in, obs)

      cover_yes / cover_own * exp_yes + cover_no / cover_own * exp_no
    }
  }
}

# Function to calculate m_S (model with feature subset S)
m_S <- function(obs, S) {
  sum(sapply(0:max(trees$Tree), function(tree) {
    if (trees[Tree == tree, all(S %in% Feature)]) {
      # For all subsets U in tree
      T <- trees[Tree == tree & Feature != "Leaf", unique(Feature)]
      sum(sapply(subsets(T), function(U) {
        if ((length(setdiff(T, S)) == 0) || (setdiff(T, S) %in% U)) {
          # TODO: Question: Why do we need that additional (-1)*?
          #(-1)^(length(S) - length(setdiff(T, U))) * recurse(tree = tree, node_id = 0, feats_in = U, obs = obs)
          (-1) * (-1)^(length(S) - length(setdiff(T, U))) * recurse(tree = tree, node_id = 0, feats_in = U, obs = obs)
        } else {
          0
        }
      }))
    } else {
      0
    }
  }))
}

# Calculate m_S for all subsets S
all_S <- subsets(colnames(x))
m_all <- sapply(all_S, m_S, obs = 1)
names(m_all) <- sapply(all_S, paste, collapse = "")

# SHAP values are the sum of the m's *1/d
d <- sapply(names(m_all), nchar)/2
shap <- sapply(colnames(x), function(col) {
  idx <- grep(col, names(m_all))
  sum(m_all[idx] / d[idx])
})
shap


# treeshap for comparison
uxg <- xgboost.unify(xg, x[1, , drop = FALSE])
treeshap(uxg, x[1, , drop = FALSE])

# Our interactions
m_all[-1] / d[-1]

# Interaction shap
treeshap(uxg, x[1, , drop = FALSE], interactions = TRUE)

# TODO: Sometimes wrong for depth >= 3, why?
# TODO: Question: No unique solution?
# TODO: Question: Our advantage? Higher-order interactions, faster? Or same speed?
# TODO: Fast implementation


