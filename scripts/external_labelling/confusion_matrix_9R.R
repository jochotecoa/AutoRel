source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'ggplot2', 'tidyverse', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21/significant_labels'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_9_path = 'data/apap_hecatos/deseq2_features_all_9vs9.rds' 
dataset_9 = dataset_9_path %>% readRDS()

colnames(dataset_9) = colnames(dataset_9) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_9[sapply(dataset_9, is.logical)] = 
  dataset_9[sapply(dataset_9, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_9))



pred_9 <- data.frame(pred = predict(model_treebag, newdata = dataset_9))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_9 = dataset_9 %>% 
  cbind.data.frame(pred_9)

dataset_9$relevant = dataset_9$pred == 'significant'
dataset_9$irrelevant = dataset_9$pred == 'nonsignificant'
dataset_9$dubious = dataset_9$pred == 'dubious'

ext_labls = 'data/apap_hecatos/external_labelling/samples_9R_Jelmer.csv' %>% 
  read.csv(row.names = 1) %>% 
  .[, -2]

colnames(ext_labls) = c('ensembl_gene_id', 'ext_label')

dataset_9_sampled = merge.data.frame(dataset_9, ext_labls, 'ensembl_gene_id')
dataset_9_sampled$ext_label %<>% as.factor()
dataset_9_sampled$pred2 = dataset_9_sampled$pred %>% 
  gsub('nonsignificant', 'irrelevant', .) %>% 
  gsub('significant', 'relevant', .) %>% 
  as.factor()


confusionMatrix(dataset_9_sampled$pred2, dataset_9_sampled$ext_label) 

genes_df = dataset_21 %>% 
  column_to_rownames('ensembl_gene_id') %>% 
  dplyr::filter(as.logical(rule_cpm_0.75_above_1)) %>% 
  dplyr::select(relevant, irrelevant, dubious)

sampled_genes = data.frame()

sampled_genes = genes_df$relevant %>% 
  which() %>% 
  sample(size = 11) %>% 
  genes_df[., ] %>% 
  rbind.data.frame(sampled_genes, .)

sampled_genes = genes_df$irrelevant %>% 
  which() %>% 
  sample(size = 12) %>% 
  genes_df[., ] %>% 
  rbind.data.frame(sampled_genes, .)

sampled_genes = genes_df$dubious %>% 
  which() %>% 
  sample(size = 11) %>% 
  genes_df[., ] %>% 
  rbind.data.frame(sampled_genes, .)


gene_ids = sampled_genes %>% 
  row.names() %>% 
  sample(size = nrow(sampled_genes), replace = F)



gene_ids %>% write.csv('output/data/apap/samples_21R.csv')

