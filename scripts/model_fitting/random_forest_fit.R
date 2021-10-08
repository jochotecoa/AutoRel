# source('script/recursive_feature_elimination/load_data_rfe.R')
forceLibrary('randomForest')

X = 'data/apap_hecatos/whole_data_preds.rds' %>% readRDS()


apap_data = cbind.data.frame(X, Y) 
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  gsub('`', '', .) %>% 
  gsub('%', 'percentage', .)
colnames(test_data) = colnames(test_data) %>% 
  gsub('`', '', .) %>% 
  gsub('%', 'percentage', .)

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

model_rf_over <- caret::train(significance ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, allowParallel = T, 
                     verboseIter = TRUE,
                     sampling = "rose")

model_rf_rose <- caret::train(significance ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, allowParallel = T, 
                     verboseIter = TRUE,
                     sampling = "smote")

model_rf_smote <- caret::train(significance ~ .,
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

final <- data.frame(actual = test_data$significance,
                          predict(model_rf, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

final_over <- data.frame(actual = test_data$significance,
                          predict(model_rf_over, newdata = test_data, type = "prob"))
final_over$predict = final_over[-1] %>% apply(1, which.max)
final_over$predict = names(final_over)[-1][final_over$predict]
final_over$predict = final_over$predict %>% as.factor()

cm_over <- confusionMatrix(final_over$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/rf/')) {
  dir.create('output/confusion_matrices/apap_21vs21/rf/', recursive = T)
}

cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/rf/over-sampling.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/rf/original.rds')
cm_under %>% saveRDS('output/confusion_matrices/apap_21vs21/rf/under-sampling.rds')

balanced_accuracies = data.frame(original = cm_original$byClass[,'Balanced Accuracy'],
                                       under = cm_under$byClass[,'Balanced Accuracy'],)

balanced_accuracies %>% 
  as.matrix() %>% 
  barplot(beside=T, 
          col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk")[1:nrow(balanced_accuracies)],
          legend = rownames(balanced_accuracies), 
          main = 'Balanced Accuracy', 
          ylim = c(0, 1))

pos_pred_values = data.frame(original = cm_original$byClass[,'Pos Pred Value'],
                                 under = cm_under$byClass[,'Pos Pred Value'])

pos_pred_values %>% 
  as.matrix() %>% 
  barplot(beside=T, 
          col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk")[1:nrow(balanced_accuracies)],
          legend = rownames(balanced_accuracies), 
          main = 'Pos Pred Value', 
          ylim = c(0, 1))



models <- list(original = model_rf,
               under = model_rf_under
               # ,
               # over = model_rf_over,
               # smote = model_rf_smote,
               # rose = model_rf_rose
               )

resampling <- resamples(models)
bwplot(resampling)
