
# source('script/recursive_feature_elimination/load_data_rpart2e.R')
forceLibrary(c('rpart')) # Needed for bagged trees

apap_data = cbind.data.frame(X, Y) 
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rpart2 <- caret::train(significance ~ .,
                                data = train_data,
                                method = "rpart2",
                                preProcess = c("scale", "center"),
                                trControl = trainControl(method = "cv", 
                                                         allowParallel = F, 
                                                         verboseIter = TRUE))

if (!dir.exists('output/trained_models/apap_21vs21/rpart2/')) {
  dir.create('output/trained_models/apap_21vs21/rpart2/', recursive = T)
}

model_rpart2 %>% saveRDS('output/trained_models/apap_21vs21/rpart2/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict(model_rpart2, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/rpart2/')) {
  dir.create('output/confusion_matrices/apap_21vs21/rpart2/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/over-sampling.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/original.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_21vs21/rpart2/under-sampling.rds')

