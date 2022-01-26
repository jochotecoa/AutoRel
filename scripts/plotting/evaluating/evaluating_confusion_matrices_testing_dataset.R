for (path_model_i in list.files('output/confusion_matrices/apap_21vs21/', 
                                full.names = T, recursive = T, pattern = '.rds')) {
  model_i_name = path_model_i %>% 
    gsub('output/confusion_matrices/apap_21vs21//', '', .) %>% 
    gsub(pattern = '.rds', replacement = '', x = .) %>% 
    gsub(pattern = '\\/', replacement = '_', x = .) %>% 
    paste0(., '_cm')
  
  model_i = path_model_i %>% readRDS()
  assign(x = model_i_name, value = model_i)
  
  acc_sig_nonsig = model_i$table['significant', 'significant'] / (model_i$table['nonsignificant', 'significant'] + model_i$table['significant', 'nonsignificant'] + model_i$table['significant', 'significant'])
  
  print(paste(model_i_name, acc_sig_nonsig))
}

resample_results <- resamples(list(
  BAM=bam_original_model,
  CSimca=CSimca_original_model,
  # KKNN=kknn_original_model,
  lssvmRadial=lssvmRadial_original_model,
  MULTINOM=multinom_original_model,
  NAIVE_BAYES=naive_bayes_original_model,
  ordinalNet=ordinalNet_original_model,
  # RF=rf_model_rf_model,
  # KKNN="rf_model_rf_over_model", 
  # KKNN="rf_model_rf_under_model", 
  # RPART2=rpart2_original_expressed_model 
  sparseLDA=sparseLDA_original_model,
  TREEBAG=treebag_original_model,
  xgbDART=xgbDART_original_model
  # RPART2=model_rpart2
)
)
