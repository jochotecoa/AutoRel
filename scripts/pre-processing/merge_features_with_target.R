source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


deseq2_dataset_all = readRDS(file = deseq2_dataset_all_path)
manual_degs = readRDS(file = manual_degs_path)

deseq2_dataset = deseq2_dataset_all %>% 
  merge.data.frame(y = manual_degs, by = 'ensembl_gene_id', all.y = T)

deseq2_dataset %>% 
  saveRDS(file = deseq2_dataset_path)

deseq2_dataset_unlabelled = 
  deseq2_dataset_all[!(deseq2_dataset_all$ensembl_gene_id %in% manual_degs$ensembl_gene_id), ]

deseq2_dataset_unlabelled %>% 
  saveRDS(file = deseq2_dataset_unlabelled_path)

