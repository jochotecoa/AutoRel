source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))


apap_dataset_path = 'data/apap_hecatos/deseq2_dataset_9vs9.rds'
apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

apap_data = apap_data %>% 
  column_to_rownames('ensembl_gene_id')

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
conf_matr_path = 'output/confusion_matrices/apap_9vs9'

registerDoMC(5)

# source('script/recursive_feature_elimination/load_data_rfe.R')
forceLibrary(c('randomForest')) # Needed for bagged trees

apap_data = apap_dataset_path %>% 
  readRDS()
# colnames(apap_data)[ncol(apap_data)] = 'significance'

# apap_data = apap_data[apap_data$baseMean > 0, ]


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]


rf_rfe_all_feats <- rfe(significance ~ .,
                   data = train_data,
                   method = "rf",
                   preProcess = c("scale", "center"),
                   rfeControl = rfeControl(functions = caretFuncs, 
                                           verbose = T, 
                                           method = "cv", 
                                           allowParallel = F),
                   ## pass options to train()
                   )

file_rds = paste0(train_mod_path, '/rf/rf_rfe_all_feats.rds')
saveRDS(rf_rfe_all_feats, file_rds)

obs_vs_pred_rfe_all_feats <- data.frame(actual = test_data$significance,
                    predict(rf_rfe_all_feats, newdata = test_data))

obs_vs_pred_rfe_all_feats$predict = obs_vs_pred_rfe_all_feats[, 2] %>% as.factor()

cm_rfe_all_feats <- confusionMatrix(obs_vs_pred_rfe_all_feats$predict, test_data$significance)

cm_rfe_all_feats$byClass[, 1:4] %>% rowMeans()
