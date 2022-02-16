source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


# Testing 21vs21 data -------------------------------------------------------


test_data_21_path = 'data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_21_test_data.rds' 
test_data_21 = test_data_21_path %>% readRDS()

colnames(test_data_21) = colnames(test_data_21) %>% 
  make.names()

# Convert all logical variables to numerical
test_data_21[sapply(test_data_21, is.logical)] = 
  test_data_21[sapply(test_data_21, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(test_data_21))

test_data_21_ori = test_data_21

test_data_21 = 
  test_data_21[, colnames(test_data_21) %in% model_treebag$coefnames]

test_data_21$significance = test_data_21_ori$significance



obsVSpred <- data.frame(actual = test_data_21$significance,
                    predict(model_treebag, newdata = test_data_21))
obsVSpred$predict = obsVSpred[, 2] %>% as.factor()

levels(test_data_21$significance) = unique(test_data_21$significance)

cm_test_data_21 <- confusionMatrix(obsVSpred$predict, obsVSpred$actual)

cm_test_data_21$byClass[, 1:4] %>% naToZero %>% rowMeans()

if (!dir.exists(paste0(conf_matr_path, '/treebag/'))) {
  dir.create(paste0(conf_matr_path, '/treebag/'), recursive = T)
}

cm_test_data_21 %>% saveRDS(paste0(conf_matr_path, '/treebag/test_data_21.rds'))


# Testing 9vs9 data -------------------------------------------------------


test_data_9_path = 'data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_9_test_data.rds' 
test_data_9 = test_data_9_path %>% readRDS()

colnames(test_data_9) = colnames(test_data_9) %>% 
  make.names()

# Convert all logical variables to numerical
test_data_9[sapply(test_data_9, is.logical)] = 
  test_data_9[sapply(test_data_9, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(test_data_9))

test_data_9_ori = test_data_9

test_data_9 = 
  test_data_9[, colnames(test_data_9) %in% model_treebag$coefnames]

test_data_9$significance = test_data_9_ori$significance



obsVSpred <- data.frame(actual = test_data_9$significance,
                        predict(model_treebag, newdata = test_data_9))
obsVSpred$predict = obsVSpred[, 2] %>% as.factor()

cm_test_data_9 <- confusionMatrix(obsVSpred$predict, test_data_9$significance)

cm_test_data_9$byClass[, 1:4] %>% naToZero %>% rowMeans()

if (!dir.exists(paste0(conf_matr_path, '/treebag/'))) {
  dir.create(paste0(conf_matr_path, '/treebag/'), recursive = T)
}

cm_test_data_9 %>% saveRDS(paste0(conf_matr_path, '/treebag/test_data_9.rds'))


# Testing 3vs3 data -------------------------------------------------------


test_data_3_path = 'data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_3_test_data.rds' 
test_data_3 = test_data_3_path %>% readRDS()

colnames(test_data_3) = colnames(test_data_3) %>% 
  make.names()

# Convert all logical variables to numerical
test_data_3[sapply(test_data_3, is.logical)] = 
  test_data_3[sapply(test_data_3, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(test_data_3))

test_data_3_ori = test_data_3

test_data_3 = 
  test_data_3[, colnames(test_data_3) %in% model_treebag$coefnames]

test_data_3$significance = test_data_3_ori$significance



obsVSpred <- data.frame(actual = test_data_3$significance,
                        predict(model_treebag, newdata = test_data_3))
obsVSpred$predict = obsVSpred[, 2] %>% as.factor()

cm_test_data_3 <- confusionMatrix(obsVSpred$predict, test_data_3$significance)

cm_test_data_3$byClass[, 1:4] %>% naToZero %>% rowMeans()

if (!dir.exists(paste0(conf_matr_path, '/treebag/'))) {
  dir.create(paste0(conf_matr_path, '/treebag/'), recursive = T)
}

cm_test_data_3 %>% saveRDS(paste0(conf_matr_path, '/treebag/test_data_3.rds'))

