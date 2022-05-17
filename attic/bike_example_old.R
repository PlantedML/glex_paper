
library(ISLR2)
library(xgboost)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(shapdecomp)

set.seed(2022)

data(Bikeshare)
bike <- data.table(Bikeshare)
bike[, mnth := as.numeric(mnth)]
bike[, hr := as.numeric(as.character(hr))]
bike[, weathersit := as.numeric(weathersit)]

#x <- as.matrix(bike[, .(season, day, hr, temp, windspeed, workingday)]) # mnth, holiday, weekday, workingday, weathersit, hum
x <- as.matrix(bike[, .(day, hr, temp, windspeed, workingday, hum)])
#x <- as.matrix(bike[, .(season, mnth, day, hr, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed)])

y <- bike$bikers

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

# Variable/Interaction importance
sort(colMeans(abs(res$m)), decreasing = TRUE)

# Find and plot strongest 2-way interactions
cols <- grep("^\\w+:\\w+$", colnames(res$m))
top_interactions <- names(sort(colMeans(abs(res$m[, cols])), decreasing = TRUE)[1:6])
intrs2way <- rbindlist(lapply(top_interactions, function(colname) {
  cols <- strsplit(colname, ":")[[1]]
  data.frame(variable = colname, x = x[, cols[1]], y = x[, cols[2]], m = res$m[, colname])
}))
ggplot(intrs2way, aes(x = x, y = y, col = m)) +
  facet_wrap(~variable, scales = "free") +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")

# Plot hr and temp with interaction
p1 <- ggplot(data.frame(hr = x[, "hr"], m = res$m[, "hr"]), aes(x = hr, y = m)) +
  geom_line() +
  theme_bw()
p2 <- ggplot(data.frame(temp = x[, "temp"], m = res$m[, "temp"]), aes(x = temp, y = m)) +
  geom_line() +
  theme_bw()
p3 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], m = res$m[, "hr:temp"]), aes(x = hr, y = temp, col = m)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")
p4 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], m = res$m[, "hr"] + res$m[, "temp"] + res$m[, "hr:temp"]), aes(x = hr, y = temp, col = m)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Plot hr, temp and workingday with interactions
p1 <- ggplot(data.frame(hr = x[, "hr"], workingday = factor(x[, "workingday"]), m = res$m[, "hr:workingday"]),
             aes(x = hr, y = m, col = workingday)) +
  geom_line() +
  theme_bw()
p2 <- ggplot(data.frame(temp = x[, "temp"], workingday = factor(x[, "workingday"]), m = res$m[, "temp:workingday"]),
             aes(x = temp, y = m, col = workingday)) +
  geom_line() +
  theme_bw()
p3 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], workingday = factor(x[, "workingday"]), m = res$m[, "hr:temp:workingday"]),
             aes(x = hr, y = temp, col = m)) +
  facet_wrap(~workingday) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")
# p4 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], workingday = factor(x[, "workingday"]),
#                         m = res$m[, "hr"] + res$m[, "temp"] + res$m[, "workingday"] +
#                           res$m[, "hr:temp"] + res$m[, "hr:workingday"] + res$m[, "temp:workingday"] +
#                           res$m[, "hr:temp:workingday"]),
#              aes(x = hr, y = temp, col = m)) +
#   facet_wrap(~workingday) +
#   geom_point() +
#   theme_bw() +
#   scale_color_gradient(low = "yellow", high = "red")
#grid.arrange(p1, p2, p3, nrow = 2, widths = c(.5, .5, 1))
plot_grid(plot_grid(p1, p2, ncol = 2), p3, nrow = 2)


# Plot SHAP, main effects, 2-way and 3-way interactions together ----------
vars <- c("hr", "temp", "workingday")

# SHAP
shaps <- rbindlist(lapply(vars, function(colname) {
  data.frame(variable = colname, value = x[, colname], shap = res$shap[, colname])
}))
p1 <- ggplot(shaps, aes(x = value, y = shap)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_point() +
  theme_bw()

# Main effects
ms <- rbindlist(lapply(vars, function(colname) {
  data.frame(variable = colname, value = x[, colname], m = res$m[, colname])
}))
p2 <- ggplot(ms, aes(x = value, y = m)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_line() +
  theme_bw()

# 2-way interactions
int2way <- rbind(data.frame(variable = "hr", value = x[, "hr"],
                            workingday = factor(x[, "workingday"]),
                            m = res$m[, "hr:workingday"]),
                 data.frame(variable = "temp", value = x[, "temp"],
                            workingday = factor(x[, "workingday"]),
                            m = res$m[, "temp:workingday"]))
p3_1 <- ggplot(int2way, aes(x = value, y = m, col = workingday)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_line() +
  theme_bw()
p3_2 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], m = res$m[, "hr:temp"]), aes(x = hr, y = temp, col = m)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_b()
p3 <- plot_grid(p3_1, p3_2, rel_widths = c(2/3, 1/3))

# 3-way interaction
p4 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"],
                        workingday = factor(x[, "workingday"]),
                        m = res$m[, "hr:temp:workingday"]),
             aes(x = hr, y = temp, col = m)) +
  facet_wrap(~workingday) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")

plot_grid(p1, p2, p3, p4, ncol = 1)
