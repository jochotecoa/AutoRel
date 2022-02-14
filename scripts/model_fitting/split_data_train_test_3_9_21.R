
if (!dir.exists('data/apap_hecatos/dataset_for_3_9_21_training_testing/')) {
  dir.create('data/apap_hecatos/dataset_for_3_9_21_training_testing/', recursive = T)
}



apap_dataset = apap_dataset_path %>% readRDS()

apap_dataset_3 = subset(apap_dataset, grepl('_3vs3', rownames(apap_dataset)))
apap_dataset_9 = subset(apap_dataset, grepl('_9vs9', rownames(apap_dataset)))
apap_dataset_21 = subset(apap_dataset, grepl('_21vs21', rownames(apap_dataset)))


apap_dataset_3_index <- createDataPartition(apap_dataset_3$significance, p = 0.75, list = FALSE)
apap_dataset_3_train_data <- apap_dataset_3[apap_dataset_3_index, ]
apap_dataset_3_test_data  <- apap_dataset_3[-apap_dataset_3_index, ]
apap_dataset_3_test_data %>% saveRDS('data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_3_test_data.rds')

apap_dataset_9_index <- createDataPartition(apap_dataset_9$significance, p = 0.75, list = FALSE)
apap_dataset_9_train_data <- apap_dataset_9[apap_dataset_9_index, ]
apap_dataset_9_test_data  <- apap_dataset_9[-apap_dataset_9_index, ]
apap_dataset_9_test_data %>% saveRDS('data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_9_test_data.rds')

apap_dataset_21_index <- createDataPartition(apap_dataset_21$significance, p = 0.75, list = FALSE)
apap_dataset_21_train_data <- apap_dataset_21[apap_dataset_21_index, ]
apap_dataset_21_test_data  <- apap_dataset_21[-apap_dataset_21_index, ]
apap_dataset_21_test_data %>% saveRDS('data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_21_test_data.rds')


apap_dataset_train_data = rbind.data.frame(
  apap_dataset_3_train_data,
  apap_dataset_9_train_data,
  apap_dataset_21_train_data
)
apap_dataset_train_data %>% saveRDS('data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_3_9_21_train_data.rds')


apap_dataset_test_data = rbind.data.frame(
  apap_dataset_3_test_data,
  apap_dataset_9_test_data,
  apap_dataset_21_test_data
)
apap_dataset_test_data %>% saveRDS('data/apap_hecatos/dataset_for_3_9_21_training_testing/apap_dataset_3_9_21_test_data.rds')
