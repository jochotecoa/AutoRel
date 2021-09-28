source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


norm_counts_features = readRDS('data/apap_hecatos/norm_counts_features.rds')
manual_res = readRDS(file = 'data/apap_hecatos/manual_res_apap_hecatos.rds')

deseq2_dataset = norm_counts_features %>% 
  rownames_to_column('ensembl_gene_id') %>% 
  merge.data.frame(y = manual_res, by = 'ensembl_gene_id', all.y = T)

saveRDS(object = deseq2_dataset, file = 'data/apap_hecatos/deseq2_dataset.rds')
