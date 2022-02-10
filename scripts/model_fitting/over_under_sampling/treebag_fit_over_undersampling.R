source('scripts/functions/functions_JOA.R')
forceLibrary(c('ipred', 'plyr', 'e1071')) # Needed for bagged trees
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))
forceLibrary(c('sparseLDA')) # Needed for bagged trees


registerDoMC(5)


apap_data = apap_dataset_path %>% 
  readRDS()


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]



# under_sampling ----------------------------------------------------------

model_sparseLDA_under <- caret::train(significance ~ .,
                                   data = train_data,
                                   method = "sparseLDA",
                                   preProcess = c("scale", "center"),
                                   trControl = trainControl(method = "cv", 
                                                            allowParallel = T, 
                                                            verboseIter = TRUE, 
                                                            sampling = "down")
)

if (!dir.exists(paste0(train_mod_path, '/sparseLDA/'))) {
  dir.create(paste0(train_mod_path, '/sparseLDA/'), recursive = T)
}

model_sparseLDA_under %>% saveRDS(paste0(train_mod_path, '/sparseLDA/sparseLDA_under_sampling.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_sparseLDA_under, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_under_sampling <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/'))) {
  dir.create(paste0(conf_matr_path, '/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/over-sampling.rds')
cm_under_sampling %>% saveRDS(paste0(conf_matr_path, '/sparseLDA_under_sampling.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/under_sampling.rds')

print(cm_under_sampling$byClass['Class: significant', 'Balanced Accuracy'])
cm_under_sampling$byClass[, 1:4] %>% rowMeans()




# over_sampling -----------------------------------------------------------



model_sparseLDA_over <- caret::train(significance ~ .,
                                  data = train_data,
                                  method = "sparseLDA",
                                  preProcess = c("scale", "center"),
                                  trControl = trainControl(method = "cv", 
                                                           allowParallel = T, 
                                                           verboseIter = TRUE, 
                                                           sampling = "up")
)

if (!dir.exists(paste0(train_mod_path, '/sparseLDA/'))) {
  dir.create(paste0(train_mod_path, '/sparseLDA/'), recursive = T)
}

model_sparseLDA_over %>% saveRDS(paste0(train_mod_path, '/sparseLDA/sparseLDA_over_sampling.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_sparseLDA_over, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_over_sampling <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/'))) {
  dir.create(paste0(conf_matr_path, '/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/over-sampling.rds')
cm_over_sampling %>% saveRDS(paste0(conf_matr_path, '/sparseLDA_over_sampling.rds'))
# cm_over %>% saveRDS(paste0(conf_matr_path, '/over_sampling.rds')

print(cm_over_sampling$byClass['Class: significant', 'Balanced Accuracy'])
cm_over_sampling$byClass[, 1:4] %>% rowMeans()
