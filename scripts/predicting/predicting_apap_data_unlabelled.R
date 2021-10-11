source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
forceLibrary(c('rpart')) # Needed for bagged trees

registerDoMC(5)

deseq2_nonlablld_dataset = readRDS('data/apap_hecatos/deseq2_nonlablld_dataset.rds')
deseq2_nonlablld_dataset = deseq2_nonlablld_dataset %>% 
  column_to_rownames('ensembl_gene_id')
model_rpart2 = readRDS('output/trained_models/apap_21vs21/rpart2/original.rds')

colnames(deseq2_nonlablld_dataset) = colnames(deseq2_nonlablld_dataset) %>% 
  make.names()

deseq2_nonlablld_dataset = deseq2_nonlablld_dataset[, colnames(deseq2_nonlablld_dataset) %in% model_rpart2$coefnames]
deseq2_nonlablld_dataset[sapply(deseq2_nonlablld_dataset, is.logical)] = 
  deseq2_nonlablld_dataset[sapply(deseq2_nonlablld_dataset, is.logical)] %>% 
  sapply(as.numeric)

stopifnot(length(model_rpart2$coefnames) == ncol(deseq2_nonlablld_dataset))

unlabelled_data_predictions = data.frame(
  row.names = row.names(deseq2_nonlablld_dataset),
  significance = predict(model_rpart2, newdata = deseq2_nonlablld_dataset)
  )

norm_counts = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos.rds' %>% readRDS

unlabelled_norm_counts = merge(x = rownames_to_column(unlabelled_data_predictions), 
                               y = rownames_to_column(as.data.frame(norm_counts)), by = 'rowname') %>% 
  column_to_rownames()

saveRDS(object = unlabelled_norm_counts, 
        file = 'output/data/apap/newdata_predicted_norm_counts_21v21.rds')

for (rownaam in rownames(unlabelled_norm_counts)) { # [rownaam_i:rownaam_f]
  
  predctn = unlabelled_norm_counts[rownaam, 'significance'] 
  
  contr_cols = grep('ConDMSO', colnames(unlabelled_norm_counts))
  treatm_cols = grep('APA_The', colnames(unlabelled_norm_counts))
  
  unlabelled_norm_counts[rownaam, c(contr_cols, treatm_cols)] %>% 
    as.numeric() %>% 
    barplot(las  =2, 
            col = c(rep('gray', length(contr_cols)), 
                    rep('pink', length(treatm_cols))), 
            main = paste0(rownaam, '; predicted = ', predctn))
  print(rownaam)
  readline(prompt = "Press [enter] to continue")
}

