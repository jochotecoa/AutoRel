source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

forceLibrary(c('naivebayes')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled.rds' %>% 
  readRDS()

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]



# original ----------------------------------------------------------------



model_naive_bayes <- caret::train(significance ~ .,
                             data = train_data,
                             method = "naive_bayes",
                             preProcess = c("scale", "center"),
                             trControl = trainControl(method = "cv", 
                                                      allowParallel = T, 
                                                      verboseIter = TRUE))

if (!dir.exists('output/trained_models/apap_21vs21/naive_bayes/')) {
  dir.create('output/trained_models/apap_21vs21/naive_bayes/', recursive = T)
}

model_naive_bayes %>% saveRDS('output/trained_models/apap_21vs21/naive_bayes/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict = predict(model_naive_bayes, newdata = test_data))

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/naive_bayes/')) {
  dir.create('output/confusion_matrices/apap_21vs21/naive_bayes/', recursive = T)
}

cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/naive_bayes/original.rds')
