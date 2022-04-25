source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'ggplot2', 'tidyverse'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21/significant_labels'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_21_path = 'data/apap_hecatos/deseq2_features_all_21vs21.rds' 
dataset_21 = dataset_21_path %>% readRDS()

colnames(dataset_21) = colnames(dataset_21) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_21[sapply(dataset_21, is.logical)] = 
  dataset_21[sapply(dataset_21, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_21))



pred_21 <- data.frame(pred = predict(model_treebag, newdata = dataset_21))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_21 = dataset_21 %>% 
  cbind.data.frame(pred_21)

dataset_21$relevant = dataset_21$pred == 'significant'
dataset_21$irrelevant = dataset_21$pred == 'nonsignificant'
dataset_21$dubious = dataset_21$pred == 'dubious'

genes_df = dataset_21 %>% 
  column_to_rownames('ensembl_gene_id') %>% 
  dplyr::filter(as.logical(rule_cpm_0.75_above_1)) %>% 
  dplyr::select(relevant, irrelevant, dubious)

sampled_genes = data.frame()

sampled_genes = genes_df$relevant %>% 
  which() %>% 
  sample(size = 11) %>% 
  genes_df[., ] %>% 
  rbind.data.frame(sampled_genes, .)

sampled_genes = genes_df$irrelevant %>% 
  which() %>% 
  sample(size = 12) %>% 
  genes_df[., ] %>% 
  rbind.data.frame(sampled_genes, .)

sampled_genes = genes_df$dubious %>% 
  which() %>% 
  sample(size = 11) %>% 
  genes_df[., ] %>% 
  rbind.data.frame(sampled_genes, .)


gene_ids = sampled_genes %>% 
  row.names() %>% 
  sample(size = nrow(sampled_genes), replace = F)


deseq2_dataset_all_path = 'data/apap_hecatos/deseq2_features_all_21vs21.rds'
norm_counts_path = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos_21vs21.rds'

deseq2_dataset_all = deseq2_dataset_all_path %>% 
  readRDS()

norm_counts = norm_counts_path %>% 
  readRDS()

dir.create('output/plots/barplots/', recursive = T)

pdf('output/plots/barplots/samples_21R.pdf')

for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  


  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  par(mfrow=c(1,2), cex.axis=0.8, mar=c(8, 4, 5, 2))
  
  norm_counts[gene_id, ] %>% 
    barplot(las = 2, 
            col = c(rep('gray', length(contr_cols)), 
                    rep('pink', length(treatm_cols))), 
            main = gene_id,                         
            ylim = c(0, max(norm_counts[gene_id, ])),
            ylab = 'Normalized counts'
    )
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', length(contr_cols)), rep('APA_The', length(treatm_cols))))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'Normalized counts', main = gene_id,
          ylim = c(0, max(norm_counts[gene_id, ])))
  
  print(gene_id)
  print(grep(gene_id, gene_ids))
}

# readline(prompt = "Press [enter] to continue")
dev.off()

gene_ids %>% write.csv('output/data/apap/samples_21R.csv')

