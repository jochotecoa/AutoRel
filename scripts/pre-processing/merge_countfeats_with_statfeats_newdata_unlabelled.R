source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


norm_counts_features = readRDS('data/apap_hecatos/norm_counts_features.rds')
# manual_res = readRDS(file = 'data/apap_hecatos/manual_res_apap_hecatos.rds')
manual_degs = readRDS(file = 'data/apap_hecatos/manual_degs_apap_hecatos.rds')
all_res = readRDS(file = 'data/apap_hecatos/all_res_apap_hecatos.rds')

norm_counts_features = norm_counts_features %>% 
  rownames_to_column('ensembl_gene_id')

non_labelled_genes = 
  setdiff(x = norm_counts_features$ensembl_gene_id, 
          y = manual_degs$ensembl_gene_id)

norm_counts_features = norm_counts_features %>% 
  column_to_rownames('ensembl_gene_id')
  
norm_counts_nonlblld_features = norm_counts_features[non_labelled_genes, , F]

saveRDS(object = norm_counts_nonlblld_features, file = 'data/apap_hecatos/norm_counts_nonlblld_features.rds')

deseq2_nonlablld_dataset = norm_counts_nonlblld_features %>% 
  rownames_to_column('ensembl_gene_id') %>% 
  merge.data.frame(y = rownames_to_column(all_res, 'ensembl_gene_id'), by = 'ensembl_gene_id', all.x = T)

saveRDS(object = deseq2_nonlablld_dataset, file = 'data/apap_hecatos/deseq2_nonlablld_dataset.rds')
