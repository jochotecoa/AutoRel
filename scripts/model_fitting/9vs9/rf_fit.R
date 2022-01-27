source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

# source('script/recursive_feature_elimination/load_data_rfe.R')
forceLibrary(c('randomForest')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds' %>% 
  readRDS()

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_rf <- caret::train(significance ~ .,
                          data = train_data,
                          method = "rf",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "cv", 
                                                   allowParallel = F, 
                                                   verboseIter = TRUE))

if (!dir.exists('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/rf/')) {
  dir.create('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/rf/', recursive = T)
}

model_rf %>% saveRDS('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/rf/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict = predict(model_rf, newdata = test_data))

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_9vs9/rf/')) {
  dir.create('output/confusion_matrices/apap_9vs9/rf/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_9vs9/rf/over-sampling.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_9vs9/rf/original.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_9vs9/rf/under-sampling.rds')

