source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds'

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
conf_matr_path = 'output/confusion_matrices/apap_9vs9'

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

# colnames(train_data) = colnames(train_data) %>% 
#   make.names()
# colnames(test_data) = colnames(test_data) %>% 
#   make.names()


# under_sampling ----------------------------------------------------------

model_rf_under <- caret::train(significance ~ .,
                                   data = train_data,
                                   method = "rf",
                                   preProcess = c("scale", "center"),
                                   trControl = trainControl(method = "cv", 
                                                            allowParallel = T, 
                                                            verboseIter = TRUE, 
                                                            sampling = "down")
)

if (!dir.exists(paste0(train_mod_path, '/rf/'))) {
  dir.create(paste0(train_mod_path, '/rf/'), recursive = T)
}

model_rf_under %>% saveRDS(paste0(train_mod_path, '/rf/rf_under_sampling.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf_under, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_under_sampling <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/'))) {
  dir.create(paste0(conf_matr_path, '/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/over-sampling.rds')
cm_under_sampling %>% saveRDS(paste0(conf_matr_path, '/rf_under_sampling.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/under_sampling.rds')

print(cm_under_sampling$byClass['Class: significant', 'Balanced Accuracy'])
cm_under_sampling$byClass[, 1:4] %>% rowMeans()




# over_sampling -----------------------------------------------------------



model_rf_over <- caret::train(significance ~ .,
                                  data = train_data,
                                  method = "rf",
                                  preProcess = c("scale", "center"),
                                  trControl = trainControl(method = "cv", 
                                                           allowParallel = T, 
                                                           verboseIter = TRUE, 
                                                           sampling = "up")
)

if (!dir.exists(paste0(train_mod_path, '/rf/'))) {
  dir.create(paste0(train_mod_path, '/rf/'), recursive = T)
}

model_rf_over %>% saveRDS(paste0(train_mod_path, '/rf/rf_over_sampling.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf_over, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_over_sampling <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/'))) {
  dir.create(paste0(conf_matr_path, '/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/over-sampling.rds')
cm_over_sampling %>% saveRDS(paste0(conf_matr_path, '/rf_over_sampling.rds'))
# cm_over %>% saveRDS(paste0(conf_matr_path, '/over_sampling.rds')

print(cm_over_sampling$byClass['Class: significant', 'Balanced Accuracy'])
cm_over_sampling$byClass[, 1:4] %>% rowMeans()
