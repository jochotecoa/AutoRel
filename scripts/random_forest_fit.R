# source('script/recursive_feature_elimination/load_data_rfe.R')
forceLibrary('randomForest')

X = 'data/apap_hecatos/whole_data_preds.rds' %>% readRDS()


apap_data = cbind.data.frame(X, Y) 
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, allowParallel = T, 
                     verboseIter = TRUE,
                     sampling = "down")

model_rf_under <- caret::train(significance ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)

model_rf <- caret::train(significance ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, allowParallel = T, 
                                                  verboseIter = TRUE))
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, allowParallel = T, 
                     verboseIter = TRUE,
                     sampling = "up")

model_rf_over <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, allowParallel = T, 
                     verboseIter = TRUE,
                     sampling = "rose")

model_rf_rose <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, allowParallel = T, 
                     verboseIter = TRUE,
                     sampling = "smote")

model_rf_smote <- caret::train(classes ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)

if (!dir.exists('output/trained_models/apap_21vs21/rf/')) {
  dir.create('output/trained_models/apap_21vs21/rf/', recursive = T)
}

model_rf_under %>% saveRDS('output/trained_models/apap_21vs21/rf/model_rf_under.rds')
model_rf_smote %>% saveRDS('output/trained_models/apap_21vs21/rf/model_rf_smote.rds')
model_rf_rose %>% saveRDS('output/trained_models/apap_21vs21/rf/model_rf_smote.rds')
model_rf_over %>% saveRDS('output/trained_models/apap_21vs21/rf/model_rf_over.rds')
model_rf %>% saveRDS('output/trained_models/apap_21vs21/rf/model_rf.rds')

final_under <- data.frame(actual = test_data$significance,
                          predict(model_rf_under, newdata = test_data, type = "prob"))
final_under$predict = final_under[-1] %>% apply(1, which.max)
final_under$predict = names(final_under)[-1][final_under$predict]
final_under$predict = final_under$predict %>% as.factor()

cm_under <- confusionMatrix(final_under$predict, test_data$significance)
