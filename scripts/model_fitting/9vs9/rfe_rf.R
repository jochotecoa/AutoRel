source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


apap_dataset_path = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds'

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
conf_matr_path = 'output/confusion_matrices/apap_9vs9'

registerDoMC(5)

# source('script/recursive_feature_elimination/load_data_rfe.R')
forceLibrary(c('randomForest')) # Needed for bagged trees

apap_data = apap_dataset_path %>% 
  readRDS()
# colnames(apap_data)[ncol(apap_data)] = 'significance'

# apap_data = apap_data[apap_data$baseMean > 0, ]


index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]


rfProfile <- rfe(significance ~ .,
                   data = train_data,
                   method = "rf",
                   preProcess = c("scale", "center"),
                   rfeControl = rfeControl(functions = caretFuncs, 
                                           verbose = T, 
                                           method = "cv", 
                                           allowParallel = T),
                   ## pass options to train()
                   )

file_rds = paste0(train_mod_path, '/rf/rf_rfe_Profile.rds')
saveRDS(rfProfile, file_rds)

final <- data.frame(actual = test_data$significance,
                    predict(rfProfile, newdata = test_data))

final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

cm_original$byClass[, 1:4] %>% rowMeans()
