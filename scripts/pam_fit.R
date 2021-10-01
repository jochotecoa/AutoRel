
# source('script/recursive_feature_elimination/load_data_pame.R')
forceLibrary(c('pamr')) # Needed for bagged trees

apap_data = cbind.data.frame(X, Y) 
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_pam <- caret::train(significance ~ .,
                             data = train_data,
                             method = "pam",
                             preProcess = c("scale", "center"),
                             trControl = trainControl(method = "cv", 
                                                      allowParallel = F, 
                                                      verboseIter = TRUE))

if (!dir.exists('output/trained_models/apap_21vs21/pam/')) {
  dir.create('output/trained_models/apap_21vs21/pam/', recursive = T)
}

model_pam %>% saveRDS('output/trained_models/apap_21vs21/pam/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict(model_pam, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/pam/')) {
  dir.create('output/confusion_matrices/apap_21vs21/pam/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/pam/over-sampling.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/pam/original.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_21vs21/pam/under-sampling.rds')

