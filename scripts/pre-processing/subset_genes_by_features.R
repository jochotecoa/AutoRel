source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))

deseq2_nonlablld_dataset = 
  readRDS(file = 'data/apap_hecatos/deseq2_nonlablld_dataset.rds')

deseq2_features_padj_tq = deseq2_nonlablld_dataset %>% 
  dplyr::filter(thirdquartile_rule == F, 
                padj < 0.05) 

deseq2_features_padj_tq %>% 
  saveRDS(file = 'data/apap_hecatos/deseq2_features_padj_tq.rds')
