

library(ISLR2)
library(xgboost)
library(treeshap)
library(data.table)
library(ggplot2)
library(gridExtra)
library(shapdecomp)

#set.seed(2022)

epo <- fread("http://publicifsv.sund.ku.dk/~helene/Epo.csv")
#full <- lrm(Y~age+sex+HbBase+Treat+Resection+Receptor,data=Epo)

x <- model.matrix(~.-1, epo[, .(age, sex, HbBase, Treat, Resection, Receptor)])
y <- epo$Y

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 20)

# SHAP decomposition
res <- shapdecomp(xg, x)

# Plot SHAP
shaps <- rbindlist(lapply(colnames(x), function(colname) {
  data.frame(variable = colname, value = x[, colname], shap = res$shap[, colname])
}))
ggplot(shaps, aes(x = value, y = shap)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_point() +
  #geom_abline(intercept = 0, slope = 1, col = "red") +
  theme_bw()

# Plot m's
ms <- rbindlist(lapply(colnames(x), function(colname) {
  if (colname %in% colnames(res$m)) {
    data.frame(variable = colname, value = x[, colname], m = res$m[, colname])
  } else {
    data.frame(variable = colname, value = x[, colname], m = 0)
  }
}))
ggplot(ms, aes(x = value, y = m)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_line() +
  theme_bw()

# Find strongest 2-way interactions
cols <- grep("^\\w+:\\w+$", colnames(res$m))
top_interactions <- names(sort(colMeans(abs(res$m[, cols])), decreasing = TRUE)[1:6])

# Plot 2-way interactions
intrs2way <- rbindlist(lapply(top_interactions, function(colname) {
  cols <- strsplit(colname, ":")[[1]]
  data.frame(variable = colname, x = x[, cols[1]], y = x[, cols[2]], m = res$m[, colname])
}))
ggplot(intrs2way, aes(x = x, y = y, col = m)) +
  facet_wrap(~variable, scales = "free") +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours = terrain.colors(10))


df <- data.frame(hbBase = x[, "HbBase"], TreatPlacebo = x[, "TreatPlacebo"], m = res$shap[, "HbBase"])
ggplot(df, aes(x = hbBase, y = m, col = factor(TreatPlacebo), group = TreatPlacebo)) +
  #facet_wrap(~variable, scales = "free") +
  geom_point() +
  geom_line() +
  theme_bw() #+
  #scale_color_gradientn(colours = terrain.colors(10))


# # Find strongest 3-way interactions
# cols <- grep("^\\w+:\\w+:\\w+$", colnames(res$m))
# top_interactions <- names(sort(colMeans(abs(res$m[, cols])), decreasing = TRUE)[1:6])
#
# # Plot 3-way interactions
# intrs3way <- rbindlist(lapply(top_interactions, function(colname) {
#   cols <- strsplit(colname, ":")[[1]]
#   data.frame(variable = colname, x = x[, cols[1]], y = x[, cols[2]], m = res$m[, colname])
# }))
# ggplot(intrs3way, aes(x = x, y = y, col = m)) +
#   facet_wrap(~variable, scales = "free") +
#   geom_point() +
#   theme_bw()

# Plot season:hr:temp interaction
df <- data.frame(x[, c("season", "hr", "temp")], m = res$m[, "season:hr:temp"])
ggplot(df, aes(x = hr, y = temp, col = m)) +
  facet_wrap(~season) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "green")


