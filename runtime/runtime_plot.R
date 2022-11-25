
library(data.table)
library(ggplot2)
library(ggsci)
library(cowplot)

df_n <- readRDS("runtime_n.Rds")
df_p <- readRDS("runtime_p.Rds")
df_rounds <- readRDS("runtime_rounds.Rds")
df_depth <- readRDS("runtime_depth.Rds")

# Change names
levs <- c("xgboost", "treeshap", "glex2", "glex")
labs <- c("xgboost (2-way interactions)", "treeshap (2-way interactions)",
          "Ours (2-way interactions)", "Ours (all interactions)")
df_n[, algo := factor(algo, levels = levs, labels = labs)]
df_p[, algo := factor(algo, levels = levs, labels = labs)]
df_rounds[, algo := factor(algo, levels = levs, labels = labs)]
df_depth[, algo := factor(algo, levels = levs, labels = labs)]

# Plot n
df_n[, seconds := time/1e9]
df_n2 <- df_n[, .(seconds = median(seconds), se = sd(seconds)), by = .(n, algo)]
p_n <- ggplot(df_n2, aes(x = n, y = seconds, col = algo, shape = algo)) +
  geom_point() +
  geom_line() +
  scale_color_nejm() +
  scale_shape_manual(values = c(16, 3, 17, 15)) +
  xlab("Sample size") +
  ylab("Runtime (seconds)") +
  theme_bw()

# Plot p
df_p[, seconds := time/1e9]
df_p2 <- df_p[, .(seconds = median(seconds)), by = .(p, algo)]
p_p<- ggplot(df_p2, aes(x = p, y = seconds, col = algo, shape = algo)) +
  geom_point() +
  geom_line() +
  scale_color_nejm() +
  scale_shape_manual(values = c(16, 3, 17, 15)) +
  xlab("Dimensionality") +
  ylab("Runtime (seconds)") +
  theme_bw()

# Plot rounds
df_rounds[, seconds := time/1e9]
df_rounds2 <- df_rounds[, .(seconds = median(seconds)), by = .(rounds, algo)]
p_rounds <- ggplot(df_rounds2, aes(x = rounds, y = seconds, col = algo, shape = algo)) +
  geom_point() +
  geom_line() +
  scale_color_nejm() +
  scale_shape_manual(values = c(16, 3, 17, 15)) +
  xlab("Rounds (trees)") +
  ylab("Runtime (seconds)") +
  theme_bw()

# Plot depth
df_depth[, seconds := time/1e9]
df_depth2 <- df_depth[, .(seconds = median(seconds)), by = .(depth, algo)]
p_depth <- ggplot(df_depth2, aes(x = depth, y = seconds, col = algo, shape = algo)) +
  geom_point() +
  geom_line() +
  scale_color_nejm() +
  scale_shape_manual(values = c(16, 3, 17, 15)) +
  xlab("Tree depth") +
  ylab("Runtime (seconds)") +
  theme_bw()

# Plot all together
prow <- cowplot::plot_grid(p_n + theme(legend.position = "none"),
                   p_p + theme(legend.position = "none"),
                   p_rounds + theme(legend.position = "none"),
                   p_depth + theme(legend.position = "none"),
                   ncol = 2)

legend <- get_legend(
  p_n +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
)
cowplot::plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("runtime.pdf", width = 8, height = 8)
