source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

forceLibrary(c('naivebayes')) # Needed for bagged trees

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


model_naive_bayes <- caret::train(relevance ~ .,
                             data = train_data,
                             method = "naive_bayes",
                             preProcess = c("scale", "center"),
                             trControl = trControl)
train_mod_path
if (!dir.exists(paste0(train_mod_path, '/naive_bayes/'))) {
  dir.create(paste0(train_mod_path, '/naive_bayes/'), recursive = T)
}

model_naive_bayes %>% saveRDS(paste0(train_mod_path, '/naive_bayes/original.rds'))


final <- data.frame(actual = test_data$relevance,
                    predict(model_naive_bayes, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$relevance)

if (!dir.exists(paste0(conf_matr_path, '/naive_bayes/'))) {
  dir.create(paste0(conf_matr_path, '/naive_bayes/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/naive_bayes/over-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/naive_bayes/original.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/naive_bayes/under-sampling.rds')

