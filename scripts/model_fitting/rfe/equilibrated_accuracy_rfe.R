
apap_data = apap_dataset_path %>% readRDS()
model_original = "/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/sparseLDA/original.rds" %>% 
  readRDS()
cm_original = 'output/confusion_matrices/apap_9vs9/sparseLDA/original.rds' %>% 
  readRDS()
algorithm = 'sparseLDA'


varimps = model_original %>% 
  varImp
varimps = varimps$importance %>% 
  apply(1, max) %>% 
  sort(decreasing=T) %>% 
  names

sizes = c(seq(10, ncol(apap_data), 10))

eqacc = eqacc_list = cm_original$byClass[, 1:4] %>% naToZero %>% rowMeans


tol <- function(x) {
  y = x
  for (i in 1:length(x)) {
    y[i] = x[i] - x[i] * 1.5/100
  }
  return(y)
}

tol_eqacc = eqacc %>% tol
  

pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = length(sizes), clear = FALSE, width= 60)

for (size in sizes) {
  pb$tick()
  
  min_features = varimps[1:size]
  
  apap_data_min_feats = apap_data[, min_features]
  apap_data_min_feats$significance = apap_data[, 'significance']
  
  index <- createDataPartition(apap_data_min_feats$significance, p = 0.75, list = FALSE)
  train_data <- apap_data_min_feats[index, ]
  test_data  <- apap_data_min_feats[-index, ]
  
  colnames(train_data) = colnames(train_data) %>% 
    make.names()
  colnames(test_data) = colnames(test_data) %>% 
    make.names()
  
  
  model_i <- caret::train(significance ~ .,
                                data = train_data,
                                method = algorithm,
                                preProcess = c("scale", "center"),
                                trControl = trControl)
  
  final <- data.frame(actual = test_data$significance,
                      predict(model_i, newdata = test_data))
  final$predict = final[, 2] %>% as.factor()
  
  cm_rfe <- confusionMatrix(final$predict, test_data$significance)
  
  eqacc_temp = cm_rfe$byClass[, 1:4] %>% naToZero %>% rowMeans
  
  eqacc_list = rbind(eqacc_list, eqacc_temp)
  rownames(eqacc_list)[nrow(eqacc_list)] = paste0('size_', size)
  
  
  # print('-------------------------------------')
  # print(eqacc_temp)
  # print('-------------------------------------')
  # 
  # good_size = all(eqacc_temp > tol_eqacc)
  # 
  # if (good_size) {
  #   break()
  # }
}

print(paste('Smallest size with a good performance:', size))

model_i %>% saveRDS(paste0(train_mod_path, '/i/rfe.rds'))

if (!dir.exists(paste0(conf_matr_path, '/i/'))) {
  dir.create(paste0(conf_matr_path, '/i/'), recursive = T)
}

# cm_over %>% saveRDS(paste0(conf_matr_path, '/i/over-sampling.rds')
cm_rfe %>% saveRDS(paste0(conf_matr_path, '/i/rfe.rds'))
