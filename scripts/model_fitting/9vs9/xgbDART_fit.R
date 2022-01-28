source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

# source('script/recursive_feature_elimination/load_data_xgbDARTe.R')
forceLibrary(c('xgboost', 'plyr')) # Needed for bagged trees

apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_xgbDART <- caret::train(significance ~ .,
                             data = train_data,
                             method = "xgbDART",
                             preProcess = c("scale", "center"),
                             trControl = trControl)

if (!dir.exists(paste0(train_mod_path, '/xgbDART/'))) {
  dir.create(paste0(train_mod_path, '/xgbDART/', recursive = T))
}

model_xgbDART %>% saveRDS(paste0(train_mod_path, '/xgbDART/original.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_xgbDART, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/xgbDART/'))) {
  dir.create(paste0(conf_matr_path, '/xgbDART/', recursive = T))
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/xgbDART/over-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/xgbDART/original.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/xgbDART/under-sampling.rds')

