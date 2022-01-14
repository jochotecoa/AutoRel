source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


res = 
  readRDS(file = 'data/apap_hecatos/results_dds_deseq2_apap_hecatos.rds')
norm_counts = 
  readRDS(file = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos.rds')
deseq2_nonlablld_dataset = 
  readRDS(file = 'data/apap_hecatos/deseq2_nonlablld_dataset.rds')
deseq2_dataset = 
  readRDS(file = 'data/apap_hecatos/deseq2_dataset.rds')

deseq2_features_subsetted = deseq2_nonlablld_dataset %>% 
  dplyr::filter(threequartilediff_rule == T) 


# Barplotting -------------------------------------------------------------

res2 = res[!is.na(res$padj), ]

cts_control = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('ConDMSO'))

cts_treatment = norm_counts %>% 
  as.data.frame() %>% 
  dplyr::select(contains('APA_The'))


gene_ids = deseq2_features_subsetted["ensembl_gene_id"] %>% 
  unlist()
# gene_ids = res2[order(res2$padj, decreasing = T),] %>% rownames()

gene_id_i = grep("ENSG00000231043", gene_ids)
gene_id_f = length(gene_ids)


for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  padjv = res2[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  

  norm_counts[gene_id, ] %>% 
    barplot(las  =2, 
            col = c(rep('gray', ncol(cts_control)), 
                    rep('pink', ncol(cts_treatment))), 
            main = paste0(gene_id, '; padj = ', padjv, '; ', tq_rule))
  print(gene_id)
  readline(prompt = "Press [enter] to continue")
}

for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  padjv = res2[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  median_contr = median(norm_counts[gene_id, contr_cols])
  median_treatm = median(norm_counts[gene_id, treatm_cols])
  quant1_contr = quantile(norm_counts[gene_id, contr_cols], 0.25)
  quant3_contr = quantile(norm_counts[gene_id, contr_cols], 0.75)
  quant1_treatm = quantile(norm_counts[gene_id, treatm_cols], 0.25)
  quant3_treatm = quantile(norm_counts[gene_id, treatm_cols], 0.75)
  min_contr = min(norm_counts[gene_id, contr_cols])
  min_treatm = min(norm_counts[gene_id, treatm_cols])
  max_contr = max(norm_counts[gene_id, contr_cols])
  max_treatm = max(norm_counts[gene_id, treatm_cols])
  
  q1belmin = quant1_contr < min_treatm
  q2belq1 = median_contr < quant1_treatm
  q3belq2 = quant3_contr < median_treatm
  maxbelq3 = max_contr < quant3_treatm
  minavoq1 = min_contr > quant1_treatm
  q1avoq2 = quant1_contr > median_treatm
  q2avoq3 = median_contr > quant3_treatm
  q3avomax = quant3_contr > max_treatm
  
  
  tq_rule = as.logical(q1belmin + q2belq1 + q3belq2 + maxbelq3 + 
                         minavoq1 + q1avoq2 + q2avoq3 + q3avomax)
  
  if (tq_rule) {
    next
  }
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = gene_id)
  readline(prompt = "Press [enter] to continue")
  
}

