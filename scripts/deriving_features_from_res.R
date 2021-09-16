source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))

res = readRDS(file = 'data/apap_hecatos/results_dds_deseq2_apap_hecatos.rds')

res_df = res %>% as.data.frame()

manual_degs = readRDS(file = 'data/apap_hecatos/manual_degs_apap_hecatos.rds')

stopifnot(all(manual_degs$ensembl_gene_id %in% rownames(res_df)))

manual_res = res_df %>% 
  rownames_to_column('ensembl_gene_id') %>% 
  merge.data.frame(y = manual_degs, by = 'ensembl_gene_id')

manual_res$log2FoldChange[is.na(manual_res$log2FoldChange)] = 0
manual_res$lfcSE[is.na(manual_res$lfcSE)] = 1
manual_res$stat[is.na(manual_res$stat)] = 0
manual_res$pvalue[is.na(manual_res$pvalue)] = 1
manual_res$padj[is.na(manual_res$pvalue)] = 1


a = manual_res %>% 
  dplyr::filter(pvalue > 0.1, baseMean > 1)

a[which.min(a$pvalue), ]

a = manual_res[is.na(manual_res$pvalue) & !is.na(manual_res$padj), ]
