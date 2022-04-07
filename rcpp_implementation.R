
library(data.table)
library(Rcpp)

sourceCpp("matrix_implementation.cpp")

myshap_rcpp <- function(xg, x) {
  trees <- xgboost::xgb.model.dt.tree(model = xg, use_int_id = TRUE)

  # Function to get all subsets of set
  subsets <- function(x) {
    do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
  }

  # Convert features to numerics (leaf = 0)
  trees[, Feature_num := as.numeric(factor(Feature, levels = c("Leaf", colnames(x)))) - 1]

  # Calculate matrices for each tree
  mats <- lapply(0:max(trees$Tree), function(tree) {
    tree_info <- trees[Tree == tree, ]

    T <- setdiff(tree_info[, sort(unique(Feature_num))], 0)
    U <- subsets(T)
    mat <- recurse(x, tree_info$Feature_num, tree_info$Split, tree_info$Yes, tree_info$No,
                   tree_info$Quality, tree_info$Cover, U, 0)
    colnames(mat) <- sapply(U, function(u) {
      paste(colnames(x)[u], collapse = ":")
    })
    mat
  })


  # TODO: sort(unique(Feature)) only works if features in alphabetical order?
  # Function to calculate m_S (model without feature subset S)
  m_S <- function(S) {
    rowSums(sapply(0:max(trees$Tree), function(tree) {
      if (trees[Tree == tree, all(S %in% Feature)]) {
        # For all subsets U in tree
        T <- trees[Tree == tree & Feature != "Leaf", sort(unique(Feature))]
        rowSums(sapply(subsets(T), function(U) {
          if ((length(setdiff(T, S)) == 0) || all(setdiff(T, S) %in% U)) {
            colname <- paste(U, collapse = ":")
            if (nchar(colname) == 0) {
              colname <- 1
            }
            (-1)^(length(S) - length(setdiff(T, U))) * mats[[tree+1]][, colname]
          } else {
            rep(0, nrow(x))
          }
        }))
      } else {
        rep(0, nrow(x))
      }
    }))
  }

  # Calculate m_S for all subsets S
  all_S <- subsets(colnames(x))
  m_all <- sapply(all_S, m_S)
  colnames(m_all) <- sapply(all_S, paste, collapse = ":")
  d <- lengths(regmatches(colnames(m_all), gregexpr(":", colnames(m_all)))) + 1

  # Return main effects and interactions
  # Overall feature effect is sum of all elements where feature is involved
  #m_all[, -1] / d[-1]
  sweep(m_all[, -1], MARGIN = 2, d[-1], "/")
}
