source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))
forceLibrary(c('rpart')) # Needed for bagged trees

registerDoMC(5)


source('scripts/pre-processing/merge_countfeats_with_statfeats_newdata_unlabelled.R')

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

unlabelled_data_predictions_probs = data.frame(
  row.names = row.names(deseq2_nonlablld_dataset),
  significance = predict(model_rpart2, newdata = deseq2_nonlablld_dataset, 
                         type = "prob")
)


norm_counts = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos.rds' %>% readRDS

predicted_new_data_norm_counts = merge(x = rownames_to_column(unlabelled_data_predictions), 
                               y = rownames_to_column(as.data.frame(norm_counts)), by = 'rowname') %>% 
  column_to_rownames()

predicted_new_data_norm_counts = merge(x = rownames_to_column(predicted_new_data_norm_counts), 
                               y = rownames_to_column(unlabelled_data_predictions_probs), by = 'rowname') %>% 
  column_to_rownames()


saveRDS(object = predicted_new_data_norm_counts, 
        file = 'output/data/apap/newdata_predicted_norm_counts_21v21.rds')

predicted_new_data_norm_counts = readRDS('output/data/apap/newdata_predicted_norm_counts_21v21.rds')

predicted_new_data_norm_counts =  
  merge(x = rownames_to_column(predicted_new_data_norm_counts), 
        y = rownames_to_column(deseq2_nonlablld_dataset['padj']), by = 'rowname') %>% 
  column_to_rownames

for (rownaam in rownames(predicted_new_data_norm_counts)[order(predicted_new_data_norm_counts$padj)]){ #[grep('ENSG00000054277', rownames(predicted_new_data_norm_counts)):nrow(predicted_new_data_norm_counts)]) { # [rownaam_i:rownaam_f]

  predicted_new_data_norm_counts_i = predicted_new_data_norm_counts[rownaam, ]

  predctn = predicted_new_data_norm_counts_i['significance'] %>% unlist()
  padj = predicted_new_data_norm_counts_i['padj'] %>% unlist()
  

  contr_cols = grep('ConDMSO', colnames(predicted_new_data_norm_counts))
  treatm_cols = grep('APA_The', colnames(predicted_new_data_norm_counts))

  prob_dub_i = predicted_new_data_norm_counts_i$significance.dubious
  prob_non_i = predicted_new_data_norm_counts_i$significance.nonsignificant
  prob_sig_i = predicted_new_data_norm_counts_i$significance.significant

  par(mfrow = c(1,2))

  predicted_new_data_norm_counts_i[c(contr_cols, treatm_cols)] %>%
    as.numeric() %>%
    barplot(las  =2,
            names.arg = colnames(predicted_new_data_norm_counts)[c(contr_cols, treatm_cols)],
            col = c(rep('gray', length(contr_cols)),
                    rep('pink', length(treatm_cols))),
            main = paste0(rownaam, '; predicted = ', predctn, '; padj = '), 
            sub = paste(' ; dub =', prob_dub_i, '; non =', prob_non_i, '; sig =', prob_sig_i))
  print(rownaam)

  data_bxplt = predicted_new_data_norm_counts_i[c(contr_cols, treatm_cols)] %>%
    as.data.frame() %>%
    t %>%
    cbind(c(rep('ConDMSO', length(contr_cols)), rep('APA_The', length(treatm_cols))))
  data_bxplt = data.frame(ConDMSO = unlist(data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1]),
                          APA_The = unlist(data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])) %>%
    apply(2, as.numeric)

  boxplot(x = data_bxplt,
          ylab = 'normalized_counts',
          main = paste0(rownaam, '; predicted = ', predctn,
                        paste(' ; dub =', prob_dub_i, '; non =', prob_non_i, ';
                              sig =', prob_sig_i)), col = c('gray', 'pink'))





  readline(prompt = "Press [enter] to continue")
}

manually_pred = read.csv('data/apap_hecatos/predicted_manually_curated_genes_12102021.csv', header = F)
colnames(manually_pred) = c('rowname', 'significance_man')
manually_pred$significance_man[(manually_pred$significance_man %in% levels(manually_pred$significance_man)[1])] = NA

semipredicted_new_data = unlabelled_data_predictions %>% 
  rownames_to_column %>% merge.data.frame(y = manually_pred, by = 'rowname')

semipredicted_new_data$significance_man[is.na(semipredicted_new_data$significance_man)] = 
  semipredicted_new_data$significance[is.na(semipredicted_new_data$significance_man)]

semipredicted_new_data$significance_man = 
  semipredicted_new_data$significance_man %>% 
  droplevels()

semipredicted_new_data = semipredicted_new_data[-2]

colnames(semipredicted_new_data) = c('ensembl_gene_id', 'significance')

manual_degs = readRDS(file = 'data/apap_hecatos/manual_degs_apap_hecatos.rds')

new_manual_degs = rbind(manual_degs, semipredicted_new_data)

stopifnot(nrow(new_manual_degs) == nrow(unique(new_manual_degs)))

new_manual_degs %>% saveRDS(file = 'data/apap_hecatos/manual_degs_apap_hecatos.rds')

