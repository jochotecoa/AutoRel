source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
forceLibrary(c('randomForest')) # Needed for bagged trees

registerDoMC(5)

apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

apap_data[sapply(apap_data, is.logical)] = apap_data[sapply(apap_data, is.logical)] %>% 
  sapply(as.numeric)
apap_data$significance = apap_data$significance %>% 
  as.factor()

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]


treebag_rfe <- rfe(significance ~ .,
                        data = train_data,
                        method = "treebag",
                        sizes = seq(10, ncol(train_data), 10),
                        preProcess = c("scale", "center"),
                        rfeControl = rfeControl(functions = treebagFuncs, 
                                                verbose = T, 
                                                method = "cv", 
                                                allowParallel = T)
                        ## pass options to train()
)

saveRDS(treebag_rfe, rfe_path)

pred_test_data_rfe <- data.frame(actual = test_data$significance,
                                           predict(treebag_rfe, newdata = test_data))

pred_test_data_rfe$predict = pred_test_data_rfe[, 2] %>% as.factor()

cm_rfe <- confusionMatrix(pred_test_data_rfe$predict, test_data$significance)

cm_rfe %>% saveRDS(conf_matr_rfe_path)


cm_rfe$byClass[, 1:4] %>% naToZero %>% rowMeans() %>% print()
