source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
forceLibrary(c('randomForest')) # Needed for bagged trees

registerDoMC(5)

apap_dataset_path = 'data/apap_hecatos/deseq2_dataset_21vs21.rds'
apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

apap_data = apap_data %>% 
  column_to_rownames('ensembl_gene_id')
apap_data[sapply(apap_data, is.logical)] = apap_data[sapply(apap_data, is.logical)] %>% 
  sapply(as.numeric)
apap_data$significance = apap_data$significance %>% 
  as.factor()

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21'
conf_matr_path = 'output/confusion_matrices/apap_21vs21'


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]


rf_rfe_all_feats <- rfe(significance ~ .,
                   data = train_data,
                   method = "rf",
                   sizes = seq(10, 160, 10),
                   preProcess = c("scale", "center"),
                   rfeControl = rfeControl(functions = rfFuncs, 
                                           verbose = T, 
                                           method = "cv", 
                                           allowParallel = F)
                   ## pass options to train()
                   )

file_rds = paste0(train_mod_path, '/rf/rf_rfe_all_feats.rds')
saveRDS(rf_rfe_all_feats, file_rds)

pred_test_data_rfe_all_feats <- data.frame(actual = test_data$significance,
                    predict(rf_rfe_all_feats, newdata = test_data))

pred_test_data_rfe_all_feats$predict = pred_test_data_rfe_all_feats[, 2] %>% as.factor()

cm_rfe_all_feats <- confusionMatrix(pred_test_data_rfe_all_feats$predict, test_data$significance)

cm_rfe_all_feats$byClass[, 1:4] %>% rowMeans()
