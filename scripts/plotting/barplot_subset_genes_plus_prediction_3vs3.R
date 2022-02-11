source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))

norm_counts_path = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos_3vs3.rds'

deseq2_nonlablld_dataset = 
  'data/apap_hecatos/deseq2_dataset_unlabelled_3vs3.rds' %>% 
  readRDS()


norm_counts = norm_counts_path %>% 
  readRDS()



# Barplotting -------------------------------------------------------------

# res2 = res[!is.na(res$padj), ]

cts_control = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('ConDMSO'))

cts_treatment = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('APA_The'))

model_rf = readRDS('/ngs-data-2/analysis/juan/autosign/trained_models/apap_3vs3/rf/original.rds')


deseq2_features_subs = deseq2_nonlablld_dataset %>%
  remove_rownames() %>%
  column_to_rownames('ensembl_gene_id')  %>% 
  dplyr::filter(
    # `onequartilediff_rule` == T,
    `quartilediff_score` == 9,
    # padj < 1
    )

colnames(deseq2_features_subs) = colnames(deseq2_features_subs) %>% 
  make.names()

# Convert all logical variables to numerical
deseq2_features_subs[sapply(deseq2_features_subs, is.logical)] = deseq2_features_subs[sapply(deseq2_features_subs, is.logical)] %>% sapply(as.numeric)


deseq2_features_subs = 
  deseq2_features_subs[, colnames(deseq2_features_subs) %in% model_rf$coefnames]


unlabelled_predicted <- data.frame(predict = 
                                     predict(model_rf, 
                                             newdata = 
                                               deseq2_features_subs), 
                                   row.names = 
                                     row.names(deseq2_features_subs))

unlabelled_predicted = cbind(unlabelled_predicted, deseq2_features_subs)


gene_ids = unlabelled_predicted %>% 
  rownames()

gene_id_i = grep("ENSG00000185559", gene_ids)
gene_id_f = length(gene_ids)


for (gene_id in gene_ids[1:6]) { # [gene_id_i:gene_id_f]
  
  
  
  padjv = unlabelled_predicted[gene_id, 'padj'] 
  
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
                         unlabelled_predicted[gene_id, 'rule_cpm_0.75_above_1'],
                         unlabelled_predicted[gene_id, 'predict'],
                         unlabelled_predicted[gene_id, 'twoquartilediff_rule'],
                         unlabelled_predicted[gene_id, 'threequartilediff_rule'],
                         unlabelled_predicted[gene_id, 'fourquartilediff_rule']
                         ),
            ylim = c(0, max(norm_counts[gene_id, ], na.rm = T))
            )
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = gene_id, 
          ylim = c(0, max(data_bxplt, na.rm = T)))
  
  print(gene_id)
  print(grep(gene_id, gene_ids))
  readline(prompt = "Press [enter] to continue")
}
