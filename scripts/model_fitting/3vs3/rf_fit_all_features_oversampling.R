source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
# source('script/recursive_feature_elimination/load_data_rf_all_featse.R')
forceLibrary(c('randomForest')) # Needed for bagged trees

apap_dataset_path = 'data/apap_hecatos/deseq2_dataset_9vs9.rds'

trControl = trainControl(method = "cv", 
                         allowParallel = T, 
                         verboseIter = TRUE,
                         sampling = "up")


apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

apap_data = apap_data %>% 
  column_to_rownames('ensembl_gene_id')

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rf_all_feats_ovrsmplng <- caret::train(significance ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = trControl)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
if (!dir.exists(paste0(train_mod_path, '/rf/all_features_oversampling'))) {
  dir.create(paste0(train_mod_path, '/rf/all_features_oversampling'), recursive = T)
}

model_rf_all_feats_ovrsmplng %>% saveRDS(paste0(train_mod_path, '/rf/all_features_oversampling.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf_all_feats_ovrsmplng, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_all_feats_ovrsmplng <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/rf/all_features_oversampling'))) {
  dir.create(paste0(conf_matr_path, '/rf/all_features_oversampling'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/rf/all_features_oversamplingover-sampling.rds')
cm_all_feats_ovrsmplng %>% saveRDS(paste0(conf_matr_path, '/rf/all_features_oversampling.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/rf/all_features_oversamplingunder-sampling.rds')


print(cm_all_feats_ovrsmplng$byClass['Class: significant', 'Balanced Accuracy'])
cm_all_feats_ovrsmplng$byClass[, 1:4] %>% rowMeans()
