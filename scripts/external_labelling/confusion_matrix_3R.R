source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'ggplot2', 'tidyverse', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21/significant_labels'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_3_path = 'data/apap_hecatos/deseq2_features_all_3vs3.rds' 
dataset_3 = dataset_3_path %>% readRDS()

colnames(dataset_3) = colnames(dataset_3) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_3[sapply(dataset_3, is.logical)] = 
  dataset_3[sapply(dataset_3, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_3))



pred_3 <- data.frame(pred = predict(model_treebag, newdata = dataset_3))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_3 = dataset_3 %>% 
  cbind.data.frame(pred_3)

dataset_3$relevant = dataset_3$pred == 'significant'
dataset_3$irrelevant = dataset_3$pred == 'nonsignificant'
dataset_3$dubious = dataset_3$pred == 'dubious'

ext_labls = 'data/apap_hecatos/external_labelling/samples_3R_Jelmer.csv' %>% 
  read.csv(row.names = 1) 

colnames(ext_labls) = c('ensembl_gene_id', 'ext_label')

dataset_3_sampled = merge.data.frame(dataset_3, ext_labls, 'ensembl_gene_id')
dataset_3_sampled$ext_label %<>% as.factor()
dataset_3_sampled$pred2 = dataset_3_sampled$pred %>% 
  gsub('nonsignificant', 'irrelevant', .) %>% 
  gsub('significant', 'relevant', .) %>% 
  as.factor()


confusionMatrix(dataset_3_sampled$pred2, dataset_3_sampled$ext_label) 
