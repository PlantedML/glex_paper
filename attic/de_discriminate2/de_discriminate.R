library(batchtools)
library(ggplot2)

set.seed(42)

# Registry ----------------------------------------------------------------
reg_name <- "de_discriminate2"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE)
makeExperimentRegistry(file.dir = reg_dir)

# Problems -----------------------------------------------------------
source("problems.R")
addProblem(name = "compas", fun = data_compas, seed = 43)
addProblem(name = "credit", fun = data_credit, seed = 44)
addProblem(name = "adult", fun = data_adult, seed = 45)

# Algorithms -----------------------------------------------------------
source("algorithms.R")
addAlgorithm(name = "shap_decomp", fun = shap_decomp)

# Experiments -----------------------------------------------------------
prob_design <- NULL
algo_design <- list(shap_decomp = expand.grid(max_depth = 5,
                                              eta = .5,
                                              nrounds = 10))
addExperiments(prob_design, algo_design, repls = 30)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
#testJob(id = 1)

# Submit -----------------------------------------------------------
submitJobs()
waitForJobs()

# Get results -------------------------------------------------------------
res <-  flatten(ijoin(reduceResultsDataTable(), getJobPars()))
saveRDS(res, "de_discriminate.Rds")

# Plot results -------------------------------------------------------------
df <- melt(res, measure.vars = c("full", "refit", "reduced"),
           variable.name = "Method", value.name = "MSE")
df[, Problem := factor(problem, levels = c("adult", "compas", "credit"),
                       labels = c("Adult", "COMPAS", "German credit"))]
ggplot(df, aes(x = Method, y = MSE)) +
  facet_wrap(~ Problem, scales = "free") +
  geom_boxplot(fill = "lightgrey") +
  #geom_hline(yintercept = 0, col = "red") +
  theme_bw() +
  ylab("Test statistic of conditional independence test")
ggsave("de_discriminate.pdf", width = 10, height = 5)
