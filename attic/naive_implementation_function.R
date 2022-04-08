

myshap <- function(xg, x) {
  trees <- xgb.model.dt.tree(model = xg, use_int_id = TRUE)

  # Function to get all subsets of set
  subsets <- function(x) {
    do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
  }

  # Recursively go through trees
  # This calculates the expectation of a tree when the given features are in the coalition
  recurse <- function(tree, node_id, feats_out, obs) {
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

      if (!(feat %in% feats_out)) {
        # If splitting variable is in, split
        recurse(tree, next_id, feats_out, obs)
      } else {
        # If not, weighted average
        yes <- trees[Tree == tree & Node == node_id, Yes]
        no <- trees[Tree == tree & Node == node_id, No]
        cover_own <- trees[Tree == tree & Node == node_id, Cover]
        cover_yes <- trees[Tree == tree & Node == yes, Cover]
        cover_no <- trees[Tree == tree & Node == no, Cover]

        exp_yes <- recurse(tree, yes, feats_out, obs)
        exp_no <- recurse(tree, no, feats_out, obs)

        cover_yes / cover_own * exp_yes + cover_no / cover_own * exp_no
      }
    }
  }

  # Function to calculate m_S (model without feature subset S)
  m_S <- function(obs, S) {
    sum(sapply(0:max(trees$Tree), function(tree) {
      if (trees[Tree == tree, all(S %in% Feature)]) {
        # For all subsets U in tree
        T <- trees[Tree == tree & Feature != "Leaf", unique(Feature)]
        sum(sapply(subsets(T), function(U) {
          if ((length(setdiff(T, S)) == 0) || all(setdiff(T, S) %in% U)) {
            (-1)^(length(S) - length(setdiff(T, U))) * recurse(tree = tree, node_id = 0, feats_out = U, obs = obs)
          } else {
            0
          }
        }))
      } else {
        0
      }
    }))
  }

  myshap <- function(obs) {
    # Calculate m_S for all subsets S
    all_S <- subsets(colnames(x))
    m_all <- sapply(all_S, m_S, obs = obs)
    names(m_all) <- sapply(all_S, paste, collapse = "")
    d <- sapply(names(m_all), nchar)/2

    # Return main effects and interactions
    # Overall feature effect is sum of all elements where feature is involved
    m_all[-1] / d[-1]
  }

  sapply(1:nrow(x), myshap)
}

