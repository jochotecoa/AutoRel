source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

# source('script/recursive_feature_elimination/load_data_ordinalNete.R')
forceLibrary(c('ordinalNet', 'plyr')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds' %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_ordinalNet <- caret::train(significance ~ .,
                                data = train_data,
                                method = "ordinalNet",
                                preProcess = c("scale", "center"),
                                trControl = trainControl(method = "cv", 
                                                         allowParallel = F, 
                                                         verboseIter = TRUE))

if (!dir.exists('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/ordinalNet/')) {
  dir.create('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/ordinalNet/', recursive = T)
}

model_ordinalNet %>% saveRDS('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/ordinalNet/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict(model_ordinalNet, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_9vs9/ordinalNet/')) {
  dir.create('output/confusion_matrices/apap_9vs9/ordinalNet/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_9vs9/ordinalNet/over-sampling.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_9vs9/ordinalNet/original.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_9vs9/ordinalNet/under-sampling.rds')

