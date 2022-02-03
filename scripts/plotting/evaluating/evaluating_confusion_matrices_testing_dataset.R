for (path_cm_i in list.files('output/confusion_matrices/apap_9vs9/', 
                                full.names = T, recursive = T, pattern = '.rds')) {
  cm_i_name = path_cm_i %>% 
    gsub('output/confusion_matrices/apap_9vs9//', '', .) %>% 
    gsub(pattern = '.rds', replacement = '', x = .) %>% 
    gsub(pattern = '\\/', replacement = '_', x = .) %>% 
    paste0(., '_cm')
  
  cm_i = path_cm_i %>% readRDS()
  assign(x = cm_i_name, value = cm_i)
  
  # acc_sig_nonsig = cm_i$table['significant', 'significant'] / (cm_i$table['nonsignificant', 'significant'] + cm_i$table['significant', 'nonsignificant'] + cm_i$table['significant', 'significant'])
  
  print(cm_i_name)
  
  cm_i$byClass['Class: significant', 1:4] %>% naToZero %>% print
  
  readline(prompt = "Press [enter] to continue")
  
}

resample_results <- resamples(list(
  BAM=bam_original_cm,
  CSimca=CSimca_original_cm,
  # KKNN=kknn_original_cm,
  lssvmRadial=lssvmRadial_original_cm,
  MULTINOM=multinom_original_cm,
  NAIVE_BAYES=naive_bayes_original_cm,
  ordinalNet=ordinalNet_original_cm,
  # RF=rf_cm_rf_cm,
  # KKNN="rf_cm_rf_over_cm", 
  # KKNN="rf_cm_rf_under_cm", 
  # RPART2=rpart2_original_expressed_cm 
  sparseLDA=sparseLDA_original_cm,
  TREEBAG=treebag_original_cm,
  xgbDART=xgbDART_original_cm
  # RPART2=cm_rpart2
)
)


ggplot(a, aes(x=Reference, y=Prediction, fill=Freq)) + 
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette="Greens", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  labs(title = "Value distribution") + # using a title instead
  geom_text(aes(label=Freq), color="black") + # printing values
  theme(text = element_text(size = 20))
