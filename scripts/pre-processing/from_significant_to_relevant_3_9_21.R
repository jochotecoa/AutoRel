library('tidyverse')

deseq2_dataset_2_path = 'data/apap_hecatos/dataset_preprocessed_apap_3_9_21.rds'

sign_dataset = readRDS(deseq2_dataset_2_path)
rel_dataset = sign_dataset

saveRDS(object = sign_dataset, file = 'data/apap_hecatos/dataset_preprocessed_apap_3_9_21_label_significant.rds')

rel_dataset$relevance = rel_dataset$significance %>% 
  gsub(pattern = 'nonsignificant', replacement = 'irrelevant') %>% 
  gsub(pattern = 'significant', replacement = 'relevant')

rel_dataset = rel_dataset[, -grep(pattern = 'significance', x = colnames(rel_dataset))]

saveRDS(object = rel_dataset, file = deseq2_dataset_2_path)
