source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
# source('script/recursive_feature_elimination/load_data_rf_16_featse.R')
forceLibrary(c('randomForest')) # Needed for bagged trees

apap_dataset_path = 'data/apap_hecatos/deseq2_dataset_9vs9.rds'

trControl = trainControl(method = "cv", 
                         allowParallel = T, 
                         verboseIter = TRUE)


apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'
apap_data = apap_data %>% 
  column_to_rownames('ensembl_gene_id')
colnames(apap_data) = colnames(apap_data) %>% 
  make.names()
apap_data[sapply(apap_data, is.logical)] = apap_data[sapply(apap_data, is.logical)] %>% sapply(as.numeric)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
model_rf_all_feats = readRDS(paste0(train_mod_path, '/rf/all_features.rds'))
feats_16 = model_rf_all_feats %>% 
  varImp 
feats_16 = feats_16$importance[order(feats_16$importance$Overall, decreasing=T),,F ] %>% 
  head(16) %>% 
  rownames

apap_data = apap_data %>% 
  dplyr::select(all_of(feats_16), significance)



index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rf_16_feats <- caret::train(significance ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = trControl)

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
if (!dir.exists(paste0(train_mod_path, '/rf/16_features'))) {
  dir.create(paste0(train_mod_path, '/rf/16_features'), recursive = T)
}

model_rf_16_feats %>% saveRDS(paste0(train_mod_path, '/rf/16_features.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf_16_feats, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/rf/16_features'))) {
  dir.create(paste0(conf_matr_path, '/rf/16_features'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/rf/16_featuresover-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/rf/16_features.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/rf/16_featuresunder-sampling.rds')


print(cm_original$byClass['Class: significant', 'Balanced Accuracy'])
cm_original$byClass[, 1:4] %>% rowMeans()
