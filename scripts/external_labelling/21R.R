source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'ggplot2', 'tidyverse', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21/significant_labels'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_21_path = 'data/apap_hecatos/deseq2_features_all_21vs21.rds' 
dataset_21 = dataset_21_path %>% readRDS()

colnames(dataset_21) = colnames(dataset_21) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_21[sapply(dataset_21, is.logical)] = 
  dataset_21[sapply(dataset_21, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_21))



pred_21 <- data.frame(pred = predict(model_treebag, newdata = dataset_21))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_21 = dataset_21 %>% 
  cbind.data.frame(pred_21)

dataset_21$relevant = dataset_21$pred == 'significant'
dataset_21$irrelevant = dataset_21$pred == 'nonsignificant'
dataset_21$dubious = dataset_21$pred == 'dubious'

ext_labls = 'data/apap_hecatos/external_labelling/samples_21R_Jelmer.csv' %>% 
  read.csv(row.names = 1) %>% 
  .[, -2]

colnames(ext_labls) = c('ensembl_gene_id', 'ext_label')

dataset_21_sampled = merge.data.frame(dataset_21, ext_labls, 'ensembl_gene_id')
dataset_21_sampled$ext_label %<>% as.factor()
dataset_21_sampled$pred2 = dataset_21_sampled$pred %>% 
  gsub('nonsignificant', 'irrelevant', .) %>% 
  gsub('significant', 'relevant', .) %>% 
  as.factor()
  

confusionMatrix(dataset_21_sampled$pred2, dataset_21_sampled$ext_label) 
