source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21/significant_labels'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_9_path = 'data/apap_hecatos/deseq2_features_all_9vs9.rds' 
dataset_9 = dataset_9_path %>% readRDS()

colnames(dataset_9) = colnames(dataset_9) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_9[sapply(dataset_9, is.logical)] = 
  dataset_9[sapply(dataset_9, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_9))



pred_9 <- data.frame(pred = predict(model_treebag, newdata = dataset_9))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_9 = dataset_9 %>% 
  cbind.data.frame(pred_9)

dataset_9$fdr_0.05 = dataset_9$padj < 0.05
dataset_9$fdr_0.05_l2fc_1.5 = (dataset_9$padj < 0.05) & (abs(dataset_9$log2FoldChange) > 1.5)
dataset_9$fdr_0.01 = dataset_9$padj < 0.01
dataset_9$fdr_0.01_l2fc_1.5 = (dataset_9$padj < 0.01) & (abs(dataset_9$log2FoldChange) > 1.5)

r_odaf_degs = 'data/r_odaf/Output/R-ODAF_APAP_9_DESeq2_RNA-Seq_APA_vs_ConDMSO_FDR_0.01_DEG_table.txt' %>% 
  read.table() %>% 
  row.names()

dataset_9$r_odaf = dataset_9$ensembl_gene_id %in% r_odaf_degs

dataset_9$relevant = dataset_9$pred == 'significant'


library(UpSetR)
degs_df = dataset_9[, c('fdr_0.05', 'fdr_0.05_l2fc_1.5', 'fdr_0.01', 'fdr_0.01_l2fc_1.5', 'r_odaf', 'relevant')]
degs_df = degs_df %>% 
  apply(2, as.integer) %>% 
  as.data.frame()

colnames(degs_df) = colnames(degs_df) %>% 
  gsub(pattern = 'fdr_', replacement = 'FDR<') %>% 
  gsub(pattern = '_l2fc_', replacement = ' & L2FC>') %>% 
  gsub(pattern = 'r_odaf', replacement = 'R-ODAF') %>% 
  gsub(pattern = 'relevant', replacement = 'Relevant') 


dir.create('output/plots/upset/', recursive = T)

tiff("output/plots/upset/comparison_rodaf_relevant_9vs9.tiff", units="in", width=10, height=5, res=300)

upset(degs_df, 
      nsets = ncol(degs_df),
      order.by='freq', 
      decreasing = T, 
      mb.ratio = c(0.7, 0.3),
      number.angles = 0, 
      text.scale = c(1.5, 1.5, 1.5, 1, 1.5, 1.5), #c(intersection size title, intersection
                          # size tick labels, set size title, set size tick labels, set
                          # names, numbers above bars)
      point.size = 2.8, 
      line.size = 1)

dev.off()
