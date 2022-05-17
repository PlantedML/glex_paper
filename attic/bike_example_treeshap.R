
library(ISLR2)
library(xgboost)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(treeshap)

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

# treeshap
uxg <- xgboost.unify(xg, x)
ts <- treeshap(uxg, x, interactions = TRUE)

# Plot hr and temp with interaction
p1 <- ggplot(data.frame(hr = x[, "hr"], m = ts$interactions["hr", "hr", ]), aes(x = hr, y = m)) +
  geom_point() +
  theme_bw()
p2 <- ggplot(data.frame(temp = x[, "temp"], m = ts$interactions["temp", "temp", ]), aes(x = temp, y = m)) +
  geom_point() +
  theme_bw()
p3 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], m = ts$interactions["temp", "hr", ] + ts$interactions["hr", "temp", ]), aes(x = hr, y = temp, col = m)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")
p4 <- ggplot(data.frame(hr = x[, "hr"], temp = x[, "temp"], m = ts$interactions["hr", "hr", ] + ts$interactions["temp", "temp", ] + ts$interactions["temp", "hr", ] + ts$interactions["hr", "temp", ]), aes(x = hr, y = temp, col = m)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "yellow", high = "red")
grid.arrange(p1, p2, p3, p4, nrow = 2)
