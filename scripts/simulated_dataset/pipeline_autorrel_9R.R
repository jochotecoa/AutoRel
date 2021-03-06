

n_replicates = 100


library('progress')
library('caret')

## ETA
pb2 <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = n_replicates, show_after = 0, force = T)

conf_matr_all_df = data.frame()
conf_matr_all_sign_df = data.frame()
conf_matr_all_rel_2_df = data.frame()

for (i in seq_len(n_replicates)) {
  pb2$tick()
  
  source('scripts/simulated_dataset/generate_spsimseq_9R.R')
  source('scripts/simulated_dataset/deseq2.R')
  
  source('scripts/simulated_dataset/confusion_matrix_population_vs_sampling_pvalues.R')
  
  if (!dir.exists('data/simulated_data/9R')) {
    dir.create('data/simulated_data/9R', recursive = T)
  }
  norm_counts_path = 'data/simulated_data/9R/norm_counts.rds'
  norm_counts_features_path = 'data/simulated_data/9R/norm_counts_features.rds'
  
  contrast_group = 'Group'
  
  saveRDS(file = norm_counts_path, norm_counts)
  source('scripts/simulated_dataset/deriving_features_from_normalized_counts.R')
  
  res_dds_path = 'data/simulated_data/9R/results_dds_deseq2.rds'
  all_res_path = 'data/simulated_data/9R/all_res.rds'
  saveRDS(object = res, file = res_dds_path)
  
  source('scripts/pre-processing/deriving_features_from_res.R')
  
  deseq2_dataset_all_path = 'data/simulated_data/9R/deseq2_features_all.rds'
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
  
  pred_1 = pred
  pred_1$pred_logi = grepl('^significant', pred_1$pred) %>% 
    as.factor()
  pred_1$pred_logi_2 = grepl('nonsignificant', pred_1$pred)
  pred_1$pred_logi_2 = !pred_1$pred_logi_2
  pred_1$pred_logi_2 %<>% as.factor()
  
  pred_rowdata = merge(res_rowdata, pred_1, 'rowname')
  
  pred_rowdata$significant[is.na(pred_rowdata$significant)] = F
  
  if (levels(pred_rowdata$DE.ind) != levels(pred_rowdata$pred_logi) | levels(pred_rowdata$DE.ind) != levels(pred_rowdata$pred_logi_2) |levels(pred_rowdata$DE.ind) != levels(pred_rowdata$significant)) {
    warning('Some prediction did not share levels in iteration ', i)
    levels(pred_rowdata$pred_logi) = levels(pred_rowdata$pred_logi_2) = levels(pred_rowdata$significant) = levels(pred_rowdata$DE.ind)
  }
  
  conf_matr_rel = confusionMatrix(pred_rowdata$pred_logi, pred_rowdata$DE.ind, positive = 'TRUE')
  conf_matr_rel_2 = confusionMatrix(pred_rowdata$pred_logi_2, pred_rowdata$DE.ind, positive = 'TRUE')
  conf_matr_sign = confusionMatrix(pred_rowdata$significant, pred_rowdata$DE.ind, positive = 'TRUE')
  
  overall_df = conf_matr_rel$overall %>% 
    as.data.frame() %>% 
    t
  byClass_df = conf_matr_rel$byClass %>% 
    as.data.frame() %>% 
    t
  conf_matr_df = cbind.data.frame(overall_df, byClass_df)
  rownames(conf_matr_df) = i
  conf_matr_all_df = conf_matr_all_df %>% 
    rbind.data.frame(conf_matr_df)
  
  overall_sign_df = conf_matr_sign$overall %>% 
    as.data.frame() %>% 
    t
  byClass_sign_df = conf_matr_sign$byClass %>% 
    as.data.frame() %>% 
    t
  conf_matr_sign_df = cbind.data.frame(overall_sign_df, byClass_sign_df)
  rownames(conf_matr_sign_df) = i
  conf_matr_all_sign_df = conf_matr_all_sign_df %>% 
    rbind.data.frame(conf_matr_sign_df)
  
  overall_rel_2_df = conf_matr_rel_2$overall %>% 
    as.data.frame() %>% 
    t
  byClass_rel_2_df = conf_matr_rel_2$byClass %>% 
    as.data.frame() %>% 
    t
  conf_matr_rel_2_df = cbind.data.frame(overall_rel_2_df, byClass_rel_2_df)
  rownames(conf_matr_rel_2_df) = i
  conf_matr_all_rel_2_df = conf_matr_all_rel_2_df %>% 
    rbind.data.frame(conf_matr_rel_2_df)
  
  
  
  
}

if (!dir.exists('output/simulated_data/9R/')) {
  dir.create('output/simulated_data/9R/', recursive = T)
}

saveRDS(conf_matr_all_df, 'output/simulated_data/9R/relevant_performance.rds')
saveRDS(conf_matr_all_rel_2_df, 'output/simulated_data/9R/rel_dub_performance.rds')
saveRDS(conf_matr_all_sign_df, 'output/simulated_data/9R/significant_performance.rds')


rel_sign_ratio = conf_matr_all_df/conf_matr_all_sign_df
rel_2_sign_ratio = conf_matr_all_rel_2_df/conf_matr_all_sign_df


