
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

  # All subsets S (that appear in any of the trees)
  all_S <- unique(do.call(c,lapply(0:max(trees$Tree), function(tree) {
    subsets(trees[Tree == tree & Feature_num > 0, sort(unique(Feature_num))])
  })))
  m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
  colnames(m_all) <- sapply(all_S, function(s) {
    paste(colnames(x)[s], collapse = ":")
  })

  # Prepare features and subsets per tree
  tree_feats <- lapply(0:max(trees$Tree), function(tree) {
    trees[Tree == tree & Feature != "Leaf", sort(unique(Feature_num))]
  })
  tree_subsets <- lapply(tree_feats, function(T) {
    subsets(T)
  })

  # TODO: Faster if we also do this loop in C++?
  for (S in all_S) {
    colname <- paste(colnames(x)[S], collapse = ":")
    if (nchar(colname) == 0) {
      colnum <- 1
    } else {
      colnum <- which(colnames(m_all) == colname)
    }

    for (tree in 0:max(trees$Tree)) {
      T <- tree_feats[[tree+1]]
      if (all(S %in% T)) {
        contribute(mats[[tree+1]], m_all, S, T, tree_subsets[[tree+1]], colnum-1)
      }
    }
  }

  d <- lengths(regmatches(colnames(m_all), gregexpr(":", colnames(m_all)))) + 1

  # Return main effects and interactions
  # Overall feature effect is sum of all elements where feature is involved
  #m_all[, -1] / d[-1]
  sweep(m_all[, -1], MARGIN = 2, d[-1], "/")
}
