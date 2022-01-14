source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


deseq2_dataset_all = readRDS(file = 'data/apap_hecatos/deseq2_features_all.rds')
manual_degs = readRDS(file = 'data/apap_hecatos/manual_degs_apap_hecatos.rds')

deseq2_dataset = deseq2_dataset_all %>% 
  merge.data.frame(y = manual_degs, by = 'ensembl_gene_id', all.y = T)

deseq2_dataset %>% 
  saveRDS(file = 'data/apap_hecatos/deseq2_dataset.rds')

deseq2_dataset_unlabelled = 
  deseq2_dataset_all[!(deseq2_dataset_all$ensembl_gene_id %in% manual_degs$ensembl_gene_id), ]

deseq2_dataset_unlabelled %>% 
  saveRDS(file = 'data/apap_hecatos/deseq2_dataset_unlabelled.rds')

