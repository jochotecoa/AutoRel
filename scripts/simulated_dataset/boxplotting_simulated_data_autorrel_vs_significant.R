library('dplyr')
# 50 replicates -----------------------------------------------------------



conf_matr_all_df = readRDS('output/simulated_data/50R/relevant_performance.rds')
conf_matr_all_rel_2_df = 
  readRDS('output/simulated_data/50R/rel_dub_performance.rds')
conf_matr_all_sign_df = 
  readRDS('output/simulated_data/50R/significant_performance.rds')


rel_sign_ratio = conf_matr_all_df/conf_matr_all_sign_df
rel_2_sign_ratio = conf_matr_all_rel_2_df/conf_matr_all_sign_df




if (!dir.exists('output/plots/boxplots/simulated_datasets/50R/')) {
  dir.create('output/plots/boxplots/simulated_datasets/50R/', recursive = T)
}

png(
  filename = 
    'output/plots/boxplots/simulated_datasets/50R/relevant_versus_significant_genes.png',
  width = 960,
  height = 960
  )
par(mar = c(10, 5, 5, 5))
rel_sign_ratio[, c(1, 2, 8:11, 18)] %>% 
  boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()


png(
  filename = 
    'output/plots/boxplots/simulated_datasets/50R/relevant&dubious_versus_significant_genes.png',
  width = 960,
  height = 960
  )

par(mar = c(10, 5, 5, 5))
rel_2_sign_ratio[, c(1, 2, 8:11, 18)] %>% 
  boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()

# 21 replicates -----------------------------------------------------------



conf_matr_all_df = readRDS('output/simulated_data/21R/relevant_performance.rds')
conf_matr_all_rel_2_df = 
  readRDS('output/simulated_data/21R/rel_dub_performance.rds')
conf_matr_all_sign_df = 
  readRDS('output/simulated_data/21R/significant_performance.rds')


rel_sign_ratio = conf_matr_all_df/conf_matr_all_sign_df
rel_2_sign_ratio = conf_matr_all_rel_2_df/conf_matr_all_sign_df




if (!dir.exists('output/plots/boxplots/simulated_datasets/21R/')) {
  dir.create('output/plots/boxplots/simulated_datasets/21R/', recursive = T)
}

png(
  filename = 
    'output/plots/boxplots/simulated_datasets/21R/relevant_versus_significant_genes.png',
  width = 960,
  height = 960
)
par(mar = c(10, 5, 5, 5))
rel_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()


png(
  filename = 
    'output/plots/boxplots/simulated_datasets/21R/relevant&dubious_versus_significant_genes.png',
  width = 960,
  height = 960
)

par(mar = c(10, 5, 5, 5))
rel_2_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()

# 9 replicates -----------------------------------------------------------



conf_matr_all_df = readRDS('output/simulated_data/9R/relevant_performance.rds')
conf_matr_all_rel_2_df = 
  readRDS('output/simulated_data/9R/rel_dub_performance.rds')
conf_matr_all_sign_df = 
  readRDS('output/simulated_data/9R/significant_performance.rds')


rel_sign_ratio = conf_matr_all_df/conf_matr_all_sign_df
rel_2_sign_ratio = conf_matr_all_rel_2_df/conf_matr_all_sign_df




if (!dir.exists('output/plots/boxplots/simulated_datasets/9R/')) {
  dir.create('output/plots/boxplots/simulated_datasets/9R/', recursive = T)
}

png(
  filename = 
    'output/plots/boxplots/simulated_datasets/9R/relevant_versus_significant_genes.png',
  width = 960,
  height = 960
)
par(mar = c(10, 5, 5, 5))
rel_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()


png(
  filename = 
    'output/plots/boxplots/simulated_datasets/9R/relevant&dubious_versus_significant_genes.png',
  width = 960,
  height = 960
)

par(mar = c(10, 5, 5, 5))
rel_2_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()

# 3 replicates -----------------------------------------------------------



conf_matr_all_df = readRDS('output/simulated_data/3R/relevant_performance.rds')
conf_matr_all_rel_2_df = 
  readRDS('output/simulated_data/3R/rel_dub_performance.rds')
conf_matr_all_sign_df = 
  readRDS('output/simulated_data/3R/significant_performance.rds')


rel_sign_ratio = conf_matr_all_df/conf_matr_all_sign_df
rel_2_sign_ratio = conf_matr_all_rel_2_df/conf_matr_all_sign_df




if (!dir.exists('output/plots/boxplots/simulated_datasets/3R/')) {
  dir.create('output/plots/boxplots/simulated_datasets/3R/', recursive = T)
}

png(
  filename = 
    'output/plots/boxplots/simulated_datasets/3R/relevant_versus_significant_genes.png',
  width = 960,
  height = 960
)
par(mar = c(10, 5, 5, 5))
rel_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()


png(
  filename = 
    'output/plots/boxplots/simulated_datasets/3R/relevant&dubious_versus_significant_genes.png',
  width = 960,
  height = 960
)

par(mar = c(10, 5, 5, 5))
rel_2_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2), ylab = 'Relevant / Significant')
abline(1, 0)
dev.off()
