source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

# source('script/recursive_feature_elimination/load_data_multinome.R')
forceLibrary(c('nnet')) # Needed for Penalized Multinomial Regression

apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

if (!exists('train_data') | !exists('test_data')) {
  index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
  train_data <- apap_data[index, ]
  test_data  <- apap_data[-index, ]
}

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_multinom <- caret::train(significance ~ .,
                                data = train_data,
                                method = "multinom",
                                preProcess = c("scale", "center"),
                                trControl = trControl)

if (!dir.exists(paste0(train_mod_path, '/multinom/'))) {
  dir.create(paste0(train_mod_path, '/multinom/'), recursive = T)
}

model_multinom %>% saveRDS(paste0(train_mod_path, '/multinom/original.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_multinom, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/multinom/'))) {
  dir.create(paste0(conf_matr_path, '/multinom/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/multinom/over-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/multinom/original.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/multinom/under-sampling.rds')

