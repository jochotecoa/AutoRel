source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

# load the data
# file_X = 'data/apap_hecatos/whole_data_preds_expressed.rds'
# file_Y = 'data/apap_hecatos/whole_data_target_expressed.rds'
# 
# X = readRDS(file = file_X)
# Y = readRDS(file = file_Y) %>% 
#   unlist()

# source('script/recursive_feature_elimination/load_data_rpart2e.R')
forceLibrary(c('rpart')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled.rds' %>% 
  readRDS()
# colnames(apap_data)[ncol(apap_data)] = 'significance'

apap_data_expressed = apap_data[apap_data$baseMean > 0, ]


index <- createDataPartition(apap_data_expressed$significance, p = 0.75, list = FALSE)
train_data <- apap_data_expressed[index, ]
test_data  <- apap_data_expressed[-index, ]

# colnames(train_data) = colnames(train_data) %>% 
#   make.names()
# colnames(test_data) = colnames(test_data) %>% 
#   make.names()


# original ----------------------------------------------------------------



model_rpart2 <- caret::train(significance ~ .,
                             data = train_data,
                             method = "rpart2",
                             preProcess = c("scale", "center"),
                             trControl = trainControl(method = "cv", 
                                                      allowParallel = T, 
                                                      verboseIter = TRUE))

if (!dir.exists('output/trained_models/apap_21vs21/rpart2/')) {
  dir.create('output/trained_models/apap_21vs21/rpart2/', recursive = T)
}

model_rpart2 %>% saveRDS('output/trained_models/apap_21vs21/rpart2/original_expressed.rds')


final <- data.frame(actual = test_data$significance,
                    predict(model_rpart2, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/rpart2/')) {
  dir.create('output/confusion_matrices/apap_21vs21/rpart2/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/over-sampling_expressed.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/original_expressed.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/under_sampling_expressed.rds')


# under_sampling ----------------------------------------------------------

model_rpart2_under <- caret::train(significance ~ .,
                                   data = train_data,
                                   method = "rpart2",
                                   preProcess = c("scale", "center"),
                                   trControl = trainControl(method = "cv", 
                                                            allowParallel = T, 
                                                            verboseIter = TRUE, 
                                                            sampling = "down")
)

if (!dir.exists('output/trained_models/apap_21vs21/rpart2/')) {
  dir.create('output/trained_models/apap_21vs21/rpart2/', recursive = T)
}

model_rpart2_under %>% saveRDS('output/trained_models/apap_21vs21/rpart2/under_sampling_expressed.rds')


final <- data.frame(actual = test_data$significance,
                    predict(model_rpart2_under, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_under_sampling <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/rpart2/')) {
  dir.create('output/confusion_matrices/apap_21vs21/rpart2/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/over-sampling_expressed.rds')
cm_under_sampling %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/under_sampling_expressed.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/under_sampling_expressed.rds')





# over_sampling -----------------------------------------------------------



model_rpart2_over <- caret::train(significance ~ .,
                                  data = train_data,
                                  method = "rpart2",
                                  preProcess = c("scale", "center"),
                                  trControl = trainControl(method = "cv", 
                                                           allowParallel = T, 
                                                           verboseIter = TRUE, 
                                                           sampling = "up")
)

if (!dir.exists('output/trained_models/apap_21vs21/rpart2/')) {
  dir.create('output/trained_models/apap_21vs21/rpart2/', recursive = T)
}

model_rpart2_over %>% saveRDS('output/trained_models/apap_21vs21/rpart2/over_sampling_expressed.rds')


final <- data.frame(actual = test_data$significance,
                    predict = predict(model_rpart2_over, newdata = test_data))
# final$predict = final[-1] %>% apply(1, which.max)
# final$predict = names(final)[-1][final$predict]
# final$predict = final$predict %>% as.factor()

cm_over_sampling <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/rpart2/')) {
  dir.create('output/confusion_matrices/apap_21vs21/rpart2/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/over-sampling_expressed.rds')
cm_over_sampling %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/over_sampling_expressed.rds')
# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/over_sampling_expressed.rds')


