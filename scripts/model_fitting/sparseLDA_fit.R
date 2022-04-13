source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

# source('script/recursive_feature_elimination/load_data_sparseLDAe.R')
forceLibrary(c('sparseLDA')) # Needed for bagged trees

apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'relevance'

if (!exists('train_data') | !exists('test_data')) {
  index <- createDataPartition(apap_data$relevance, p = 0.75, list = FALSE)
  train_data <- apap_data[index, ]
  test_data  <- apap_data[-index, ]
}

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_sparseLDA <- caret::train(relevance ~ .,
                          data = train_data,
                          method = "sparseLDA",
                          preProcess = c("scale", "center"),
                          trControl = trControl)

if (!dir.exists(paste0(train_mod_path, '/sparseLDA/'))) {
  dir.create(paste0(train_mod_path, '/sparseLDA/'), recursive = T)
}

model_sparseLDA %>% saveRDS(paste0(train_mod_path, '/sparseLDA/original.rds'))


final <- data.frame(actual = test_data$relevance,
                    predict(model_sparseLDA, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$relevance)

if (!dir.exists(paste0(conf_matr_path, '/sparseLDA/'))) {
  dir.create(paste0(conf_matr_path, '/sparseLDA/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/sparseLDA/over-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/sparseLDA/original.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/sparseLDA/under-sampling.rds')

