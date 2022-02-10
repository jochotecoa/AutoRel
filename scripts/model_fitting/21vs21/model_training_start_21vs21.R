source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

apap_dataset_path = 'data/apap_hecatos/dataset_preprocessed_apap_21vs21.rds'

trControl = trainControl(method = "cv", 
                         allowParallel = F, 
                         verboseIter = TRUE)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21'
conf_matr_path = 'output/confusion_matrices/apap_21vs21'

start_t = Sys.time()
source('scripts/model_fitting/CSimca_fit.R')
csi_t = Sys.time() 
print(csi_t - start_t)
source('scripts/model_fitting/naive_bayes_fit.R')
nai_t = Sys.time() 
print(nai_t - csi_t)
source('scripts/model_fitting/pam_fit.R')
pam_t = Sys.time() 
print(pam_t - nai_t)
source('scripts/model_fitting/lssvmRadial_fit.R')
lss_t = Sys.time() 
print(lss_t - pam_t)
source('scripts/model_fitting/multinom_fit.R')
mul_t = Sys.time() 
print(mul_t - lss_t)
source('scripts/model_fitting/treebag_fit.R')
tre_t = Sys.time() 
print(tre_t - mul_t)
source('scripts/model_fitting/sparseLDA_fit.R')
spa_t = Sys.time() 
print(spa_t - tre_t)
source('scripts/model_fitting/rpart2_fit.R')
rpa_t = Sys.time() 
print(rpa_t - spa_t)
source('scripts/model_fitting/kknn_fit.R')
kkn_t = Sys.time() 
print(kkn_t - rpa_t)
source('scripts/model_fitting/rf_fit.R')
rf_t = Sys.time() 
print(rf_t - kkn_t)
source('scripts/model_fitting/ordinalNet_fit.R')
ord_t = Sys.time() 
print(ord_t - rf_t)
source('scripts/model_fitting/bam_fit.R')
bam_t = Sys.time() 
print(bam_t - ord_t)
source('scripts/model_fitting/xgbDART_fit.R')
xgb_t = Sys.time() 
print(xgb_t - bam_t)

rfe_path = paste0(train_mod_path, '/rf/rf_rfe.rds')
conf_matr_rfe_path = paste0(conf_matr_path, '/rf/rf_rfe.rds')

source('scripts/model_fitting/rfe/rf_rfe.R')

apap_data_min_feats_path = 'data/apap_hecatos/dataset_labelled_X_features_21vs21.rds'
source('scripts/model_fitting/make_dataset_without_rcrsv_elmntd_featrs.R')

apap_dataset_path = apap_data_min_feats_path
train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21/after_rfe/'
conf_matr_path = 'output/confusion_matrices/apap_21vs21/after_rfe/'

source('scripts/model_fitting/rf_fit.R')

cm_original$byClass[, 1:4] %>% naToZero %>% rowMeans()

source('scripts/model_fitting/over_under_sampling/rf_fit_over_undersampling.R')

source('scripts/model_fitting/over_under_sampling/rf_fit_oversampling_randomsearch.R')

