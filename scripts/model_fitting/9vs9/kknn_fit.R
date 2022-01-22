source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

forceLibrary(c('kknn')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds' %>% 
  readRDS()

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]



# original ----------------------------------------------------------------



model_kknn <- caret::train(significance ~ .,
                             data = train_data,
                             method = "kknn",
                             preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv", 
                                                    allowParallel = T, 
                                                    verboseIter = TRUE, 
                                                    repeats = 10))

if (!dir.exists('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/kknn/')) {
  dir.create('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/kknn/', recursive = T)
}

model_kknn %>% saveRDS('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/kknn/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict = predict(model_kknn, newdata = test_data), 
                    row.names = row.names(test_data))

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_9vs9/kknn/')) {
  dir.create('output/confusion_matrices/apap_9vs9/kknn/', recursive = T)
}

cm_original %>% saveRDS('output/confusion_matrices/apap_9vs9/kknn/original.rds')
