source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

forceLibrary(c('kernlab')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled.rds' %>% 
  readRDS()

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]



# original ----------------------------------------------------------------



model_lssvmRadial <- caret::train(significance ~ .,
                             data = train_data,
                             method = "lssvmRadial",
                             preProcess = c("scale", "center"),
                             trControl = trainControl(method = "cv", 
                                                      allowParallel = T, 
                                                      verboseIter = TRUE))

if (!dir.exists('output/trained_models/apap_21vs21/lssvmRadial/')) {
  dir.create('output/trained_models/apap_21vs21/lssvmRadial/', recursive = T)
}

model_lssvmRadial %>% saveRDS('output/trained_models/apap_21vs21/lssvmRadial/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict = predict(model_lssvmRadial, newdata = test_data))

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/lssvmRadial/')) {
  dir.create('output/confusion_matrices/apap_21vs21/lssvmRadial/', recursive = T)
}

cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/lssvmRadial/original.rds')
