library(ISLR2)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("rpf.R")
source("shap_rpf.R")

set.seed(2022)

ntrees=50

# Prepare data
data(Bikeshare)
bike <- data.table(Bikeshare)
bike[, hr := as.numeric(as.character(hr))]
x <- as.matrix(bike[, .(day, hr, temp, windspeed, workingday, hum)])
y <- bike$bikers

#results from rpf
res_rpf <- rpf(X=x, Y=y, max_interaction = 4, ntrees = ntrees, splits = 30)

#obtain shap decomposition
X<-as.matrix(x)

res_shap <- shap_rpf(res_rpf, X)

# results from the decomposition
res = pred_shap_rpf(x,res_shap, shap=TRUE)

# Plot SHAP, main effects, 2-way and 3-way interactions together ----------
vars <- c(2, 3, 5)

vars_name <- c("hr", "temp", "workingday")


# SHAP
shaps <- rbindlist(lapply(1:length(vars), function(colname) {
  data.frame(variable = vars_name[colname], value = x[, vars_name[colname]], shap = res[,ncol(res)-6 + vars[colname]])
}))
p1 <- ggplot(shaps, aes(x = value, y = shap)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_point() +
  theme_bw() +
  xlab("Feature value") +
  ylab("SHAP values")

# Main effects
vars <- c(2, 3, 4)

ms <- rbindlist(lapply(1:length(vars), function(colname) {
  data.frame(variable = vars_name[colname], value = x[, vars_name[colname]], m = res[, vars[colname]+1])
}))
p2 <- ggplot(ms, aes(x = value, y = m)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_line() +
  theme_bw() +
  xlab("Feature value") +
  ylab(expression(Main~effects~italic(m[j])))

# 2-way interactions
int2way <- rbind(data.table(variable = "hr", value = x[, "hr"],
                            workingday = (x[, "workingday"]),
                            m = res[, "hr,workingday(2,5)"]),
                 data.table(variable = "temp", value = x[, "temp"],
                            workingday = (x[, "workingday"]),
                            m = res[, "temp,workingday(3,5)"]))
int2way[, workingday := factor(workingday, levels = 0:1,
                               labels = c("No working day", "Working day"))]
p3_1 <- ggplot(int2way, aes(x = value, y = m, col = workingday)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_line() +
  scale_color_manual(values = viridisLite::viridis(4)[c(2, 3)], name = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Feature value") +
  ylab(expression(2-way~interactions~italic(m[jk])))
p3_2 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], m = res[, "hr,temp(2,3)"]), aes(x = hr, y = temp, col = m)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(name = expression(italic(m[jk])))
p3 <- plot_grid(p3_1, p3_2, rel_widths = c(.55, .45))

# 3-way interaction
p4 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"],
                        workingday = factor(x[, "workingday"], levels = c("0", "1"),
                                            labels = c("No working day", "Working day")),
                        m = res[, "hr,temp,workingday(2,3,5)"]),
             aes(x = hr, y = temp, col = m)) +
  facet_wrap(~workingday) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(name = expression(italic(m[jkl]))) +
  ylab(expression(atop(3-way~interactions~italic(m[jkl]), "temp")))

title1 <- ggdraw() +
  draw_label("SHAP values",
             #fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(p1, p2, p3, p4, ncol = 1)

ggsave("bike_example.png", width = 10, height = 10.5)