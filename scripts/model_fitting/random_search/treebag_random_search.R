source('scripts/functions/functions_JOA.R')
forceLibrary(c('ipred', 'plyr', 'e1071')) # Needed for bagged trees
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


registerDoMC(5)

# source('script/recursive_feature_elimination/load_data_treebage.R')

apap_data = apap_dataset_path %>% 
  readRDS()
# colnames(apap_data)[ncol(apap_data)] = 'significance'

# apap_data = apap_data[apap_data$baseMean > 0, ]


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_treebag_random <- caret::train(significance ~ .,
                                data = train_data,
                                method = "treebag",
                                preProcess = c("scale", "center"),
                                trControl = trainControl(method = "cv", 
                                                         allowParallel = T, 
                                                         verboseIter = TRUE, 
                                                         search = "random"))

if (!dir.exists(paste0(train_mod_path, '/treebag/'))) {
  dir.create(paste0(train_mod_path, '/treebag/'), recursive = T)
}

model_treebag_random %>% saveRDS(paste0(train_mod_path, '/treebag/random_search.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_treebag_random, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_random_search <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/treebag/'))) {
  dir.create(paste0(conf_matr_path, '/treebag/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/treebag/over-sampling.rds')
cm_random_search %>% saveRDS(paste0(conf_matr_path, '/treebag/random_search.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/treebag/under-sampling.rds')

cm_random_search$byClass[, 1:4] %>% rowMeans()

