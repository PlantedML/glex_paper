
library(data.table)
library(Rcpp)

sourceCpp("matrix_implementation.cpp")

myshap_rcpp_old <- function(xg, x) {
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

  #browser()

  # All subsets S (that appear in any of the trees)
  # all_S <- unique(do.call(c,lapply(0:max(trees$Tree), function(tree) {
  #   subsets(trees[Tree == tree & Feature_num > 0, sort(unique(Feature_num))])
  # })))
  # m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
  # colnames(m_all) <- sapply(all_S, function(s) {
  #   paste(colnames(x)[s], collapse = ":")
  # })
  # for (tree in 0:max(trees$Tree)) {
  #   # All subsets in tree
  #   T <- trees[Tree == tree & Feature != "Leaf", sort(unique(Feature_num))]
  #   for (U in subsets(T)) {
  #     # Contribute to all sets U where T\S in U in T
  #     idx <- sapply(all_S, function(S) {
  #       (length(setdiff(T, S)) == 0) || all(setdiff(T, S) %in% U)
  #     })
  #
  #     # Signs
  #     signs <- sapply(all_S[idx], function(S) {
  #       (-1)^(length(S) - length(setdiff(T, U)))
  #     })
  #
  #     # Contribute
  #     colname <- paste(colnames(x)[U], collapse = ":")
  #     m_all[, idx] = m_all[, idx] + mats[[tree+1]][, colname] %*% t(signs)
  #   }
  # }

  # TODO: This is the bottleneck, also do in C++?
  # TODO: Other idea: Each matrix column contributes to several m's with different sign, go through the mats instead of the subsets?
  # Function to calculate m_S (model without feature subset S)
  m_S <- function(S) {
    rowSums(sapply(0:max(trees$Tree), function(tree) {
      if (trees[Tree == tree, all(S %in% Feature_num)]) {
        # For all subsets U in tree
        sTS <- setdiff(T, S)
        T <- trees[Tree == tree & Feature != "Leaf", sort(unique(Feature_num))]
        rowSums(sapply(subsets(T), function(U) {
          if ((length(sTS) == 0) || all(sTS %in% U)) {
            colname <- paste(colnames(x)[U], collapse = ":")
            if (nchar(colname) == 0) {
              colname <- 1
            }
            message("T ", T, ", S ", S, ", U ", U, ", sign: ", (-1)^(length(S) - length(setdiff(T, U))))
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

  # Calculate m_S for all subsets S (that appear in any of the trees)
  all_S <- unique(do.call(c,lapply(0:max(trees$Tree), function(tree) {
    subsets(trees[Tree == tree & Feature_num > 0, sort(unique(Feature_num))])
  })))
  m_all <- sapply(all_S, m_S)
  colnames(m_all) <- sapply(all_S, function(s) {
    paste(colnames(x)[s], collapse = ":")
  })
  d <- lengths(regmatches(colnames(m_all), gregexpr(":", colnames(m_all)))) + 1

  # Return main effects and interactions
  # Overall feature effect is sum of all elements where feature is involved
  #m_all[, -1] / d[-1]
  sweep(m_all[, -1], MARGIN = 2, d[-1], "/")
}
