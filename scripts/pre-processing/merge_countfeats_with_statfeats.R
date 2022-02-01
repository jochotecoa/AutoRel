source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


norm_counts_features = readRDS(norm_counts_features_path) %>% 
  rownames_to_column('ensembl_gene_id')
all_res = readRDS(file = all_res_path) %>% 
  rownames_to_column('ensembl_gene_id')

stopifnot(all(norm_counts_features$ensembl_gene_id %in% all_res$ensembl_gene_id))

deseq2_dataset_all = norm_counts_features %>% 
  merge.data.frame(y = all_res, by = 'ensembl_gene_id')

saveRDS(object = deseq2_dataset_all, 
        file = deseq2_dataset_all_path)

