source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

forceLibrary(c('kernlab')) # Needed for bagged trees

apap_data = apap_dataset_path %>% readRDS()
colnames(apap_data)[ncol(apap_data)] = 'significance'

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
train_data <- apap_data[index, ]
test_data  <- apap_data[-index, ]

colnames(train_data) = colnames(train_data) %>% 
  make.names()
colnames(test_data) = colnames(test_data) %>% 
  make.names()


model_lssvmRadial <- caret::train(significance ~ .,
                             data = train_data,
                             method = "lssvmRadial",
                             preProcess = c("scale", "center"),
                             trControl = trControl)
train_mod_path
if (!dir.exists(paste0(train_mod_path, '/lssvmRadial/'))) {
  dir.create(paste0(train_mod_path, '/lssvmRadial/'), recursive = T)
}

model_lssvmRadial %>% saveRDS(paste0(train_mod_path, '/lssvmRadial/original.rds'))


final <- data.frame(actual = test_data$significance,
                    predict(model_lssvmRadial, newdata = test_data))
final$predict = final[, 2] %>% as.factor()

cm_original <- confusionMatrix(final$predict, test_data$significance)

if (!dir.exists(paste0(conf_matr_path, '/lssvmRadial/'))) {
  dir.create(paste0(conf_matr_path, '/lssvmRadial/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/lssvmRadial/over-sampling.rds')
cm_original %>% saveRDS(paste0(conf_matr_path, '/lssvmRadial/original.rds'))
# cm_under %>% saveRDS(paste0(conf_matr_path, '/lssvmRadial/under-sampling.rds')

