# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

recurse <- function(x, feature, split, yes, no, quality, cover, U, node) {
    .Call(`_shapdecomp_recurse`, x, feature, split, yes, no, quality, cover, U, node)
}

contribute <- function(mat, m_all, S, T, T_subsets, colnum) {
    invisible(.Call(`_shapdecomp_contribute`, mat, m_all, S, T, T_subsets, colnum))
}

