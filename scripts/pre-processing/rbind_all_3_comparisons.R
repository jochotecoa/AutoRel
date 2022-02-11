
source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

deseq2_dataset_3_path = 'data/apap_hecatos/deseq2_dataset_3vs3.rds'
deseq2_dataset_9_path = 'data/apap_hecatos/deseq2_dataset_9vs9.rds'
deseq2_dataset_21_path = 'data/apap_hecatos/deseq2_dataset_21vs21.rds'

deseq2_dataset_3 = deseq2_dataset_3_path %>% readRDS()
deseq2_dataset_9 = deseq2_dataset_9_path %>% readRDS()
deseq2_dataset_21 = deseq2_dataset_21_path %>% readRDS()

deseq2_dataset_3$ensembl_gene_id = 
  deseq2_dataset_3$ensembl_gene_id %>% 
  paste0('_3vs3')

deseq2_dataset_9$ensembl_gene_id = 
  deseq2_dataset_9$ensembl_gene_id %>% 
  paste0('_9vs9')

deseq2_dataset_21$ensembl_gene_id = 
  deseq2_dataset_21$ensembl_gene_id %>% 
  paste0('_21vs21')


deseq2_dataset_3_9_21_path = 'data/apap_hecatos/deseq2_dataset_3_9_21.rds'



deseq2_dataset_3_9_21 = rbind.data.frame(deseq2_dataset_3, deseq2_dataset_9, deseq2_dataset_21)

deseq2_dataset_3_9_21 %>% saveRDS(deseq2_dataset_3_9_21_path)




