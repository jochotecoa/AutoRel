# 'scripts/apap_hecatos_9vs9_deseq2.R'
# 'scripts/pre-processing/deriving_features_from_res.R'
# 'scripts/pre-processing/deriving_features_from_normalized_counts.R'
# 'scripts/pre-processing/merge_countfeats_with_statfeats.R'
# 'scripts/plotting/barplot_subset_genes.R'
source('scripts/pre-processing/generate_database_manual_degs_9vs9.R')
source('scripts/pre-processing/merge_features_with_target_9vs9.R')
source('scripts/pre-processing/pre-processing_9vs9.R')
source('scripts/model_fitting/9vs9/kknn_fit.R')
