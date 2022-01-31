source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds'

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



source('scripts/model_fitting/9vs9/rf_fit_all_features.R')


source('scripts/model_fitting/9vs9/rf_fit_over_undersampling.R')

source('scripts/model_fitting/9vs9/rfe_rf.R')

source('scripts/model_fitting/9vs9/rf_random_search.R')

source('scripts/model_fitting/9vs9/rf_fit_all_features_oversampling.R')

source('scripts/model_fitting/9vs9/rf_rfe_all_features.R')