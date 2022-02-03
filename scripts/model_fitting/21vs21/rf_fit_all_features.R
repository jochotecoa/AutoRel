source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
# source('script/recursive_feature_elimination/load_data_rf_all_featse.R')
forceLibrary(c('randomForest')) # Needed for bagged trees


trControl = trainControl(method = "cv", 
                         allowParallel = T, 
                         verboseIter = TRUE)


apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

apap_data = apap_data %>% 
  column_to_rownames('ensembl_gene_id')

apap_data[sapply(apap_data, is.logical)] = apap_data[sapply(apap_data, is.logical)] %>% 
  sapply(as.numeric)


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rf_all_feats <- caret::train(significance ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = trControl)

if (!dir.exists(paste0(train_mod_path, '/rf/all_features'))) {
  dir.create(paste0(train_mod_path, '/rf/all_features'), recursive = T)
}

model_rf_all_feats %>% saveRDS(paste0(train_mod_path, '/rf/all_features.rds'))


predictions_test_data <- data.frame(obs = test_data$significance,
                                    pred = predict(model_rf_all_feats, 
                                                   newdata = test_data))

cm_all_features <- confusionMatrix(predictions_test_data$pred, 
                               predictions_test_data$obs)

if (!dir.exists(paste0(conf_matr_path, '/rf/all_features'))) {
  dir.create(paste0(conf_matr_path, '/rf/all_features'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/rf/all_featuresover-sampling.rds')
cm_all_features %>% saveRDS(paste0(conf_matr_path, '/rf/all_features.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/rf/all_featuresunder-sampling.rds')


print(cm_all_features$byClass['Class: significant', 'Balanced Accuracy'])
cm_all_features$byClass[, 1:4] %>% rowMeans()
