source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

# source('script/recursive_feature_elimination/load_data_rfe.R')
forceLibrary(c('randomForest')) # Needed for bagged trees

apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rf <- caret::train(significance ~ .,
                          data = train_data,
                          method = "rf",
                          preProcess = c("scale", "center"),
                          trControl = trControl)

if (!dir.exists(paste0(train_mod_path, '/rf/'))) {
  dir.create(paste0(train_mod_path, '/rf/', recursive = T))
}

model_rf %>% saveRDS(paste0(train_mod_path, '/rf/original.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/rf/'))) {
  dir.create(paste0(conf_matr_path, '/rf/', recursive = T))
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/rf/over-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/rf/original.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/rf/under-sampling.rds')

