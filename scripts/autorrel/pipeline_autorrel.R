library('progress')
library('caret')

# To run this algorithm, you need 2 files: norm_counts and res.

# This is the column name of your 'coldata' dataframe which you specified
# in DESeq2 to split between control and treatment

# contrast_group = NULL




res %<>% as.data.frame() %>% 
  rownames_to_column()

  if (!dir.exists('data/temporary_data')) {
    dir.create('data/temporary_data', recursive = T)
  }
  norm_counts_path = 'data/temporary_data/norm_counts.rds'
  norm_counts_features_path = 'data/temporary_data/norm_counts_features.rds'
  
  
  saveRDS(file = norm_counts_path, norm_counts)
  source('scripts/simulated_dataset/deriving_features_from_normalized_counts.R')
  
  res_dds_path = 'data/temporary_data/results_dds_deseq2.rds'
  all_res_path = 'data/temporary_data/all_res.rds'
  saveRDS(object = res, file = res_dds_path)
  
  source('scripts/pre-processing/deriving_features_from_res.R')
  
  deseq2_dataset_all_path = 'data/temporary_data/deseq2_features_all.rds'
  source('scripts/simulated_dataset/merge_countfeats_with_statfeats.R')
  
  train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
  
  model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))
  deseq2_dataset_all = deseq2_dataset_all_path %>% readRDS() %>% as.data.frame()
  
  colnames(deseq2_dataset_all) %<>% make.names() %>% 
    gsub('abo', 'avo', .) %>% 
    gsub('avove', 'above', .)
  
  deseq2_dataset_all[deseq2_dataset_all == T] = 1
  deseq2_dataset_all[deseq2_dataset_all == F] = 0
  
  
  unclass <- function(x) {
    return(class(unlist(x)))
  }
  
  logi_cols = deseq2_dataset_all %>% 
    sapply(class) %>% 
    as.data.frame() %>% 
    .[1,, F] %>% 
    unlist()
  
  deseq2_dataset_all[, logi_cols == 'matrix'] %<>% apply(2, as.integer)
  
  incorrect = model_treebag$coefnames[!model_treebag$coefnames %in% colnames(deseq2_dataset_all)]
  stopifnot(length(incorrect) == 0)
  
  pred <- data.frame(pred = predict(model_treebag, newdata = deseq2_dataset_all), 
                     rowname = deseq2_dataset_all$rowname)
  
  pred$pred %<>% gsub(pattern = 'nonsignificant', replacement = 'irrelevant') %>% 
    gsub(pattern = 'significant', replacement = 'relevant')
  
