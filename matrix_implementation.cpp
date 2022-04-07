#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix recurse(Rcpp::NumericMatrix& x, Rcpp::IntegerVector& feature, Rcpp::NumericVector& split,
                            Rcpp::IntegerVector& yes, Rcpp::IntegerVector& no, Rcpp::NumericVector& quality,
                            Rcpp::NumericVector& cover, std::vector<std::vector<unsigned int>>& U, unsigned int node) {

  // Start with all 0
  unsigned int n = x.nrow();
  Rcpp::NumericMatrix mat(n, U.size());

  // If leaf, just return value
  if (feature[node] == 0) {
    double pred = quality[node];
    std::fill(mat.begin(), mat.end(), pred);
  } else {
    // Call both children, they give a matrix each of all obs and subsets
    Rcpp::NumericMatrix mat_yes = recurse(x, feature, split, yes, no, quality, cover, U,  yes[node]);
    Rcpp::NumericMatrix mat_no = recurse(x, feature, split, yes, no, quality, cover, U, no[node]);

    for (unsigned int j = 0; j < U.size(); ++j) {
      // Is splitting feature out in this subset?
      bool isout = false;
      for (unsigned int k = 0; k < U[j].size(); ++k) {
        if (U[j][k] == feature[node]) {
          isout = true;
        }
      }

      if (isout) {
        // For subsets where feature is out, weighted average of left/right
        for (unsigned int i = 0; i < n; ++i) {
          mat(i, j) += cover[yes[node]] / cover[node] * mat_yes(i, j) + cover[no[node]] / cover[node] * mat_no(i, j);
        }
      } else {
        // For subsets where feature is in, split to left/right
        for (unsigned int i = 0; i < n; ++i) {
          if (x(i, feature[node]-1) <= split[node]) {
            mat(i, j) += mat_yes(i, j);
          } else {
            mat(i, j) += mat_no(i, j);
          }
        }
      }
    }
  }

  // Return combined matrix
  return(mat);
}

