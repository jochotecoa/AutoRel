
norm_counts_path = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos_9vs9.rds'
norm_counts_features_path = 'data/apap_hecatos/norm_counts_features_9vs9.rds'
dds_path = 'data/apap_hecatos/dds_deseq2_apap_hecatos_9vs9.rds'

source('scripts/pre-processing/deriving_features_from_normalized_counts.R')

res_dds_path = 'data/apap_hecatos/results_dds_deseq2_apap_hecatos_9vs9.rds'
all_res_path = 'data/apap_hecatos/all_res_apap_hecatos_9vs9.rds'
source('scripts/pre-processing/deriving_features_from_res.R')

deseq2_dataset_all_path = 'data/apap_hecatos/deseq2_features_all_9vs9.rds'
source('scripts/pre-processing/merge_countfeats_with_statfeats.R')

manual_annot_path = 'data/apap_hecatos/manual_annot_apap_hecatos_9vs9.rds'
manual_degs_path = 'data/apap_hecatos/manual_degs_apap_hecatos_9vs9.rds'
source('scripts/pre-processing/generate_database_manual_degs_9vs9.R')

deseq2_dataset_path = 'data/apap_hecatos/deseq2_dataset_9vs9.rds'
deseq2_dataset_unlabelled_path = 'data/apap_hecatos/deseq2_dataset_unlabelled_9vs9.rds'
source('scripts/pre-processing/merge_features_with_target.R')


deseq2_dataset_2_path = 'data/apap_hecatos/dataset_preprocessed_apap_9vs9.rds'
source('scripts/pre-processing/pre-processing.R')

