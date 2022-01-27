source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))


res = 
  readRDS(file = 'data/apap_hecatos/results_dds_9vs9_deseq2_apap_hecatos.rds')
norm_counts = 
  readRDS(file = 'data/apap_hecatos/norm_counts_9vs9_deseq2_apap_hecatos.rds')
deseq2_nonlablld_dataset = 
  readRDS(file = 'data/apap_hecatos/deseq2_nonlablld_dataset.rds')
deseq2_features_all =
  readRDS(file = 'data/apap_hecatos/deseq2_features_all_9vs9.rds')
# 
# deseq2_features_alletted = deseq2_nonlablld_dataset %>% 
#   dplyr::filter(onequartilediff_rule == F) 


# Barplotting -------------------------------------------------------------

res2 = res[!is.na(res$padj), ]

cts_control = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('ConDMSO'))

cts_treatment = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('APA_The'))

model_kknn = readRDS('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/kknn/original.rds')


apap_data = 'data/apap_hecatos/whole_dataset_labelled_9vs9.rds' %>% 
  readRDS()

index <- createDataPartition(apap_data$significance, p = 0.75, list = FALSE)
test_data  <- apap_data[-index, ]

final <- data.frame(obs = test_data$significance,
                    pred = predict(model_kknn, newdata = test_data), 
                    predict(model_kknn, newdata = test_data, 
                            type = "prob"),
                    row.names = row.names(test_data))


gene_ids = final[final$obs != final$pred, ] %>%
  row.names()
  
deseq2_features_all = deseq2_features_all %>% 
  column_to_rownames('ensembl_gene_id')


gene_id_i = grep("ENSG00000159459", gene_ids)
gene_id_f = length(gene_ids)


for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  padjv = deseq2_features_all[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  par(mfrow=c(1,2))
  
  norm_counts[gene_id, ] %>% 
    barplot(las = 2, 
            col = c(rep('gray', ncol(cts_control)), 
                    rep('pink', ncol(cts_treatment))), 
            main = paste(gene_id, 
                         'padj = ', format(padjv, scientific = T, digits = 3),
                         'cpm_rule:', 
                         deseq2_features_all[gene_id, 'rule_cpm_0.75_above_1'],
                         'Predicted:' ,
                         final[gene_id, 'pred'],
                         'Observed:',
                         final[gene_id, 'obs'],
                         deseq2_features_all[gene_id, 'quartilediff_score']
                         )
            )
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = gene_id)
  
  print(gene_id)
  readline(prompt = "Press [enter] to continue")
}

for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  padjv = deseq2_features_all[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = gene_id)
  print(gene_id)
  readline(prompt = "Press [enter] to continue")
  
}

