source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/whole_dataset_labelled_21vs21.rds'

trControl = trainControl(method = "cv", 
                         allowParallel = F, 
                         verboseIter = TRUE)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21'
conf_matr_path = 'output/confusion_matrices/apap_21vs21'

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



source('scripts/model_fitting/21vs21/treebag_fit_all_features.R')
