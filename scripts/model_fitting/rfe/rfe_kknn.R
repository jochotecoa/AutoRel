# kknn
# source('script/recursive_feature_elimination/load_data_rfe.R')
# KNN
forceLibrary('kknn')

rpart2Profile <- rfe(significance ~ .,
                   data = train_data,
                   method = "rpart2",
                   preProcess = c("scale", "center"),
                   rfeControl = rfeControl(functions = caretFuncs, 
                                           verbose = T, 
                                           method = "repeatedcv", 
                                           allowParallel = T),
                   ## pass options to train()
                   )
file_rds = paste0(path_output, '/kknnProfile.rds')
saveRDS(kknnProfile, file_rds)
# stopCluster(cl)
# model_rpart2 <- caret::train(
#                              trControl = trainControl(method = "repeatedcv", 
#                                                       allowParallel = T, 
#                                                       verboseIter = TRUE, 
#                                                       repeats = 10))
