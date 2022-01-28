source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds'

trControl = trainControl(method = "cv", 
                         allowParallel = F, 
                         verboseIter = TRUE)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
conf_matr_path = 'output/confusion_matrices/apap_9vs9'

source('scripts/model_fitting/9vs9/CSimca_fit.R')
source('scripts/model_fitting/9vs9/naive_bayes_fit.R')
source('scripts/model_fitting/9vs9/pam_fit.R')
source('scripts/model_fitting/9vs9/lssvmRadial_fit.R')
source('scripts/model_fitting/9vs9/multinom_fit.R')
source('scripts/model_fitting/9vs9/treebag_fit.R')
source('scripts/model_fitting/9vs9/sparseLDA_fit.R')
source('scripts/model_fitting/9vs9/rpart2_fit.R')
source('scripts/model_fitting/9vs9/kknn_fit.R')
source('scripts/model_fitting/9vs9/rf_fit.R')
