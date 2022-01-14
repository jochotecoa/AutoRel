source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


norm_counts_features = readRDS('data/apap_hecatos/norm_counts_features.rds') %>% 
  rownames_to_column('ensembl_gene_id')
all_res = readRDS(file = 'data/apap_hecatos/all_res_apap_hecatos.rds') %>% 
  rownames_to_column('ensembl_gene_id')

stopifnot(all(norm_counts_features$ensembl_gene_id %in% all_res$ensembl_gene_id))

deseq2_dataset_all = norm_counts_features %>% 
  merge.data.frame(y = all_res, by = 'ensembl_gene_id')

saveRDS(object = deseq2_dataset_all, 
        file = 'data/apap_hecatos/deseq2_features_all.rds')

