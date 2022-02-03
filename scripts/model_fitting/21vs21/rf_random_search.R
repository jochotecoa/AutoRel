source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/whole_dataset_labelled_21vs21.rds'

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21'
conf_matr_path = 'output/confusion_matrices/apap_21vs21'

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

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rf_random <- caret::train(significance ~ .,
                                data = train_data,
                                method = "rf",
                                preProcess = c("scale", "center"),
                                trControl = trainControl(method = "cv", 
                                                         allowParallel = T, 
                                                         verboseIter = TRUE, 
                                                         search = "random"))

if (!dir.exists(paste0(train_mod_path, '/rf/'))) {
  dir.create(paste0(train_mod_path, '/rf/'), recursive = T)
}

model_rf_random %>% saveRDS(paste0(train_mod_path, '/rf/random_search.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf_random, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_random_search <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/rf/'))) {
  dir.create(paste0(conf_matr_path, '/rf/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/rf/over-sampling.rds')
cm_random_search %>% saveRDS(paste0(conf_matr_path, '/rf/random_search.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/rf/under-sampling.rds')

cm_random_search$byClass[, 1:4] %>% rowMeans()

