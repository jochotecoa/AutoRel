
source('scripts/simulated_dataset/generate_spsimseq_50R.R')
source('scripts/simulated_dataset/deseq2.R')

source('scripts/simulated_dataset/confusion_matrix_population_vs_sampling_pvalues.R')

if (!dir.exists('data/simulated_data/50R')) {
  dir.create('data/simulated_data/50R', recursive = T)
}
norm_counts_path = 'data/simulated_data/50R/norm_counts.csv'
norm_counts_features_path = 'data/simulated_data/50R/norm_counts_features.rds'

contrast_group = 'Group'

saveRDS(file = norm_counts_path, norm_counts)
source('scripts/pre-processing/deriving_features_from_normalized_counts.R')

