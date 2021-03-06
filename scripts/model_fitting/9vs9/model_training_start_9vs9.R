source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/dataset_preprocessed_apap_9vs9.rds'

trControl = trainControl(method = "cv", 
                         allowParallel = F, 
                         verboseIter = TRUE)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
conf_matr_path = 'output/confusion_matrices/apap_9vs9'

source('scripts/model_fitting/CSimca_fit.R')
source('scripts/model_fitting/naive_bayes_fit.R')
source('scripts/model_fitting/pam_fit.R')
source('scripts/model_fitting/lssvmRadial_fit.R')
source('scripts/model_fitting/multinom_fit.R')
source('scripts/model_fitting/treebag_fit.R')
source('scripts/model_fitting/sparseLDA_fit.R')
source('scripts/model_fitting/rpart2_fit.R')
source('scripts/model_fitting/kknn_fit.R')
source('scripts/model_fitting/rf_fit.R')
source('scripts/model_fitting/ordinalNet_fit.R')
source('scripts/model_fitting/bam_fit.R')
source('scripts/model_fitting/xgbDART_fit.R')



rfe_path = paste0(train_mod_path, '/sparseLDA/sparseLDA_rfe.rds')
conf_matr_rfe_path = paste0(conf_matr_path, '/sparseLDA/sparseLDA_rfe.rds')

source('scripts/model_fitting/rfe/sparseLDA_rfe.R')

apap_data_min_feats_path = 'data/apap_hecatos/dataset_labelled_X_features_9vs9.rds'
source('scripts/model_fitting/make_dataset_without_rcrsv_elmntd_featrs.R')

apap_dataset_path = apap_data_min_feats_path
train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/after_rfe/'
conf_matr_path = 'output/confusion_matrices/apap_9vs9/after_rfe/'

source('scripts/model_fitting/sparseLDA_fit.R')

cm_original$byClass[, 1:4] %>% naToZero %>% rowMeans()



source('scripts/model_fitting/over_under_sampling/sparseLDA_fit_over_undersampling.R')

source('scripts/model_fitting/random_search/sparseLDA_random_search.R')

