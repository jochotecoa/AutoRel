source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


res = 
  readRDS(file = 'data/apap_hecatos/results_dds_deseq2_apap_hecatos.rds')
norm_counts = 
  readRDS(file = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos.rds')
deseq2_dataset_unlabelled = 
  readRDS(file = 'data/apap_hecatos/deseq2_dataset_unlabelled.rds')

deseq2_dataset =
  readRDS(file = 'data/apap_hecatos/deseq2_dataset.rds')

deseq2_features_subsetted = deseq2_dataset_unlabelled %>%
  dplyr::filter(onequartilediff_rule == F)


# Barplotting -------------------------------------------------------------

res2 = res[!is.na(res$padj), ]

cts_control = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('ConDMSO'))

cts_treatment = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('APA_The'))




deseq2_dataset_unlabelled = deseq2_dataset_unlabelled %>% 
  remove_rownames() %>% 
  column_to_rownames('ensembl_gene_id') %>% 
  na.omit()

colnames(deseq2_dataset_unlabelled) = colnames(deseq2_dataset_unlabelled) %>% 
  make.names()

# Convert all logical variables to numerical
deseq2_dataset_unlabelled[sapply(deseq2_dataset_unlabelled, is.logical)] = deseq2_dataset_unlabelled[sapply(deseq2_dataset_unlabelled, is.logical)] %>% sapply(as.numeric)


deseq2_dataset_unlabelled = 
  deseq2_dataset_unlabelled[, colnames(deseq2_dataset_unlabelled) %in% model_rpart2$coefnames]


unlabelled_predicted <- data.frame(predict = 
                                     predict(model_kknn, 
                                             newdata = 
                                               deseq2_dataset_unlabelled), 
                                   row.names = 
                                     row.names(deseq2_dataset_unlabelled))




deseq2_dataset = deseq2_dataset %>%
  remove_rownames() %>%
  column_to_rownames('ensembl_gene_id')

# gene_ids = 
#   manual_annot[manual_annot$significance == 'nonsignificant', "ensembl_gene_id"] 


# gene_ids = res2[order(res2$padj, decreasing = T),] %>% rownames()
gene_ids = unlabelled_predicted[unlabelled_predicted$predict == 'significant',,F] %>% 
  rownames()
deseq2_dataset_all[deseq2_dataset_all$ensembl_gene_id %in% gene_ids, ] %>% summary

gene_id_i = grep("ENSG00000100300", gene_ids)
gene_id_f = length(gene_ids)


for (gene_id in gene_ids[gene_id_i:gene_id_f]) { # [gene_id_i:gene_id_f]
  
  padjv = res2[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  
  norm_counts[gene_id, ] %>% 
    barplot(las = 2, 
            col = c(rep('gray', ncol(cts_control)), 
                    rep('pink', ncol(cts_treatment))), 
            main = paste(gene_id, 
                         '; padj = ', format(padjv, scientific = T, digits = 3), 
                         # ' actual: ', unlabelled_predicted[gene_id, 'actual'],
                         ' predicted: ', unlabelled_predicted[gene_id, 'predict'],
                         deseq2_dataset_unlabelled[gene_id, 'onequartilediff_rule'],
                         deseq2_dataset_unlabelled[gene_id, 'twoquartilediff_rule'],
                         deseq2_dataset_unlabelled[gene_id, 'threequartilediff_rule'],
                         deseq2_dataset_unlabelled[gene_id, 'fourquartilediff_rule']
            )
    )
  print(gene_id)
  readline(prompt = "Press [enter] to continue")
}

for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  padjv = res2[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = gene_id)
  readline(prompt = "Press [enter] to continue")
  
}

