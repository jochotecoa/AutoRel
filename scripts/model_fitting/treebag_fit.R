source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))
# source('script/recursive_feature_elimination/load_data_treebage.R')
forceLibrary(c('ipred', 'plyr', 'e1071')) # Needed for bagged trees

apap_data = 'data/apap_hecatos/whole_dataset_labelled.rds' %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_treebag <- caret::train(significance ~ .,
                         data = train_data,
                         method = "treebag",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "cv", 
                                                  allowParallel = F, 
                                                  verboseIter = TRUE))

if (!dir.exists('output/trained_models/apap_21vs21/treebag/')) {
  dir.create('output/trained_models/apap_21vs21/treebag/', recursive = T)
}

model_treebag %>% saveRDS('output/trained_models/apap_21vs21/treebag/original.rds')


final <- data.frame(actual = test_data$significance,
                    predict(model_treebag, newdata = test_data, type = "prob"))
final$predict = final[-1] %>% apply(1, which.max)
final$predict = names(final)[-1][final$predict]
final$predict = final$predict %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists('output/confusion_matrices/apap_21vs21/treebag/')) {
  dir.create('output/confusion_matrices/apap_21vs21/treebag/', recursive = T)
}

# cm_over %>% saveRDS('output/confusion_matrices/apap_21vs21/treebag/over-sampling.rds')
cm_original %>% saveRDS('output/confusion_matrices/apap_21vs21/treebag/original.rds')
# cm_under %>% saveRDS('output/confusion_matrices/apap_21vs21/treebag/under-sampling.rds')

