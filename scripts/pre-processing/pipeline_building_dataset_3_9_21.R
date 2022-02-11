
source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


deseq2_dataset_path = 
  deseq2_dataset_3_9_21_path = 
  'data/apap_hecatos/deseq2_dataset_3_9_21.rds'

deseq2_dataset_unlabelled_path = 'data/apap_hecatos/deseq2_dataset_unlabelled_9vs9.rds'

source('scripts/pre-processing/rbind_all_3_comparisons.R')

deseq2_dataset_2_path = 'data/apap_hecatos/dataset_preprocessed_apap_3_9_21.rds'
source('scripts/pre-processing/pre-processing.R')

