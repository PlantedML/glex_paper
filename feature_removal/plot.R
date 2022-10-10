
library(data.table)
library(ggplot2)
library(xtable)
library(ggsci)
library(viridis)

res_sim <- readRDS("res_sim.Rds")
res_adult <- readRDS("res_adult.Rds")
df <- rbind(data.table(Setting = "Simulation", res_sim),
            data.table(Setting = "Adult", res_adult))
df[, Setting := factor(Setting, levels = c("Simulation", "Adult"),
                       labels = c("Simulation", "Adult"))]

ggplot(df, aes(x = Sex, y = Prediction, fill = Model)) +
  facet_wrap(~ Setting, scales = "free") +
  geom_boxplot(outlier.shape = NA, alpha = .9) +
  scale_fill_manual(values = viridis_pal()(4)[1:3]) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("feature_removal.pdf", width = 6, height = 4)

# Median differences
medians <- df[, median(Prediction), by = .(Setting, Sex, Model)]
medians[, diff(V1), by = .(Setting, Model)]
print(xtable(medians[, diff(V1), by = .(Setting, Model)]), booktabs = TRUE)
