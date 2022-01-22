source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


res = 
  readRDS(file = 'data/apap_hecatos/results_dds_9vs9_deseq2_apap_hecatos.rds')
norm_counts = 
  readRDS(file = 'data/apap_hecatos/norm_counts_9vs9_deseq2_apap_hecatos.rds')
deseq2_nonlablld_dataset = 
  readRDS(file = 'data/apap_hecatos/deseq2_nonlablld_dataset.rds')
deseq2_features_all =
  readRDS(file = 'data/apap_hecatos/deseq2_features_all_9vs9.rds')
# 
# deseq2_features_subsetted = deseq2_nonlablld_dataset %>% 
#   dplyr::filter(onequartilediff_rule == F) 


# Barplotting -------------------------------------------------------------

res2 = res[!is.na(res$padj), ]

cts_control = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('ConDMSO'))

cts_treatment = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('APA_The'))




# gene_ids = deseq2_features_subsetted["ensembl_gene_id"] %>% 
#   unlist()


deseq2_features_subs = deseq2_features_all %>%
  remove_rownames() %>%
  column_to_rownames('ensembl_gene_id') %>%
  dplyr::filter(
    `fourquartilediff_rule` == F,
    `threequartilediff_rule` == T,
    padj > 0.3
#     rule_cpm_0.75_above_1 == F,
#     baseMean > 0
    )

colnames(deseq2_features_subs) = colnames(deseq2_features_subs) %>% 
  make.names()

# Convert all logical variables to numerical
deseq2_features_subs[sapply(deseq2_features_subs, is.logical)] = deseq2_features_subs[sapply(deseq2_features_subs, is.logical)] %>% sapply(as.numeric)


deseq2_features_subs = 
  deseq2_features_subs[, colnames(deseq2_features_subs) %in% model_rpart2$coefnames]


unlabelled_predicted <- data.frame(predict = 
                                     predict(model_kknn, 
                                             newdata = 
                                               deseq2_features_subs), 
                                   row.names = 
                                     row.names(deseq2_features_subs))


# gene_ids = 
#   manual_annot[manual_annot$significance == 'nonsignificant', "ensembl_gene_id"] 
  

# gene_ids = res2[order(res2$padj, decreasing = T),] %>% rownames()
gene_ids = unlabelled_predicted %>% 
  rownames()

gene_id_i = grep("ENSG00000185559", gene_ids)
gene_id_f = length(gene_ids)


for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  padjv = deseq2_features_subs[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  par(mfrow=c(1,2))
  
  norm_counts[gene_id, ] %>% 
    barplot(las = 2, 
            col = c(rep('gray', ncol(cts_control)), 
                    rep('pink', ncol(cts_treatment))), 
            main = paste(gene_id, 
                         '; padj = ', format(padjv, scientific = T, digits = 3),
                         'cpm_rule:', 
                         deseq2_features_subs[gene_id, 'rule_cpm_0.75_above_1'],
                         unlabelled_predicted[gene_id, 'predict']
                         # deseq2_features_subs[gene_id, 'onequartilediff_rule'],
                         # deseq2_features_subs[gene_id, 'twoquartilediff_rule'],
                         # deseq2_features_subs[gene_id, 'threequartilediff_rule'],
                         # deseq2_features_subs[gene_id, 'fourquartilediff_rule']
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
  
  padjv = deseq2_features_subs[gene_id, 'padj'] 
  
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

