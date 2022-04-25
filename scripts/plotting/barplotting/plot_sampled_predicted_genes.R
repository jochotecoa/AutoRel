source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'ggplot2', 'tidyverse'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_3_path = 'data/apap_hecatos/deseq2_features_all_3vs3.rds' 
dataset_3 = dataset_3_path %>% readRDS()

colnames(dataset_3) = colnames(dataset_3) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_3[sapply(dataset_3, is.logical)] = 
  dataset_3[sapply(dataset_3, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_3))



pred_3 <- data.frame(pred = predict(model_treebag, newdata = dataset_3))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_3 = dataset_3 %>% 
  cbind.data.frame(pred_3)

dataset_3$fdr_0.05 = dataset_3$padj < 0.05
dataset_3$fdr_0.05_l2fc_1.5 = (dataset_3$padj < 0.05) & (abs(dataset_3$log2FoldChange) > 1.5)
dataset_3$fdr_0.01 = dataset_3$padj < 0.01
dataset_3$fdr_0.01_l2fc_1.5 = (dataset_3$padj < 0.01) & (abs(dataset_3$log2FoldChange) > 1.5)

r_odaf_degs = 'data/r_odaf/Output/R-ODAF_APAP_3_DESeq2_RNA-Seq_APA_vs_ConDMSO_FDR_0.01_DEG_table.txt' %>% 
  read.table() %>% 
  row.names()

dataset_3$r_odaf = dataset_3$ensembl_gene_id %in% r_odaf_degs

dataset_3$relevant = dataset_3$pred == 'significant'


library(UpSetR)
degs_df = dataset_3[, c('fdr_0.05', 'fdr_0.05_l2fc_1.5', 'fdr_0.01', 'fdr_0.01_l2fc_1.5', 'r_odaf', 'relevant')]
degs_df = degs_df %>% 
  apply(2, as.integer) %>% 
  as.data.frame()

rownames(degs_df) = dataset_3$ensembl_gene_id

colnames(degs_df) = colnames(degs_df) %>% 
  gsub(pattern = 'fdr_', replacement = 'FDR<') %>% 
  gsub(pattern = '_l2fc_', replacement = ' & L2FC>') %>% 
  gsub(pattern = 'r_odaf', replacement = 'R-ODAF') %>% 
  gsub(pattern = 'relevant', replacement = 'Relevant') 

degs_df$groups_tog = rowSums(degs_df)

degs_df = degs_df %>% 
  dplyr::filter(Relevant == 1, groups_tog == 1)

relevant_only = dataset_3[dataset_3$ensembl_gene_id %in% rownames(degs_df), ]
outl = relevant_only$baseMean %>% outlier %>% min
relevant_only = relevant_only[relevant_only$baseMean < outl, ]
relevant_shared = dataset_3[!dataset_3$ensembl_gene_id %in% rownames(degs_df), ]
relevant_shared = relevant_shared[relevant_shared$relevant, ]
outl = relevant_shared$baseMean %>% outlier %>% min
relevant_shared = relevant_shared[relevant_shared$baseMean < outl, ]

relevant_only = relevant_only %>% 
  remove_rownames %>% 
  column_to_rownames('ensembl_gene_id')
gene_ids = relevant_only %>% 
  row.names()


deseq2_dataset_all_path = 'data/apap_hecatos/deseq2_features_all_3vs3.rds'
norm_counts_path = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos_3vs3.rds'

deseq2_dataset_all = deseq2_dataset_all_path %>% 
  readRDS()

norm_counts = norm_counts_path %>% 
  readRDS()

dir.create('output/plots/barplots/', recursive = T)



for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  tiff(paste0("output/plots/barplots/", gene_id, "_3vs3.tiff"), units="in", width=12, height=10, res=300)  
  
  padjv = relevant_only[gene_id, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  par(mfrow=c(1,2), cex.axis=0.8, mar=c(8, 4, 5, 2))
  
  norm_counts[gene_id, ] %>% 
    barplot(las = 2, 
            col = c(rep('gray', length(contr_cols)), 
                    rep('pink', length(treatm_cols))), 
            main = paste(gene_id, 
                         '; padj = ', format(padjv, scientific = T, digits = 3),
                         '; L2FC:', 
                         round(relevant_only[gene_id, 'log2FoldChange'], digits = 2)
                         ),
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
  readline(prompt = "Press [enter] to continue")
  dev.off()
}
