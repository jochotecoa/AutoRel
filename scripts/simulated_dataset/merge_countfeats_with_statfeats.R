source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


norm_counts_features = readRDS(norm_counts_features_path) %>% 
  rownames_to_column()
all_res = readRDS(file = all_res_path) 

rowname_col = all_res %>% colnames %>% grepl('rowname', .) %>% any

if (!rowname_col) {
  all_res = all_res %>% 
    rownames_to_column()
}

stopifnot(all(norm_counts_features$rowname %in% all_res$rowname))

deseq2_dataset_all = norm_counts_features %>% 
  merge.data.frame(y = all_res, by = 'rowname')

saveRDS(object = deseq2_dataset_all, 
        file = deseq2_dataset_all_path)

