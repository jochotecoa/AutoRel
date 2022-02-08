source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


registerDoMC(5)

forceLibrary(c('randomForest')) # Needed for bagged trees

apap_data = apap_dataset_path %>% 
  readRDS()


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]



# oversampling_randomsearch -----------------------------------------------------------



model_rf_over_random <- caret::train(significance ~ .,
                                  data = train_data,
                                  method = "rf",
                                  preProcess = c("scale", "center"),
                                  trControl = trainControl(method = "cv", 
                                                           allowParallel = T, 
                                                           verboseIter = TRUE, 
                                                           sampling = "up", #oversampling
                                                           search = "random") #random search
)

if (!dir.exists(paste0(train_mod_path, '/rf/'))) {
  dir.create(paste0(train_mod_path, '/rf/'), recursive = T)
}

model_rf_over_random %>% 
  saveRDS(paste0(train_mod_path, '/rf/rf_oversampling_randomsearch.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_rf_over_random, 
                            newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_oversampling_randomsearch <- 
  confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/'))) {
  dir.create(paste0(conf_matr_path, '/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/over-sampling.rds')
cm_oversampling_randomsearch %>% 
  saveRDS(paste0(conf_matr_path, '/rf_oversampling_randomsearch.rds'))
# cm_over %>% saveRDS(paste0(conf_matr_path, '/oversampling_randomsearch.rds')

print(cm_oversampling_randomsearch$byClass['Class: significant', 'Balanced Accuracy'])
cm_oversampling_randomsearch$byClass[, 1:4] %>% rowMeans()
