source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'ggplot2', 'tidyverse'))


norm_counts_path = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos_21vs21.rds'

norm_counts = norm_counts_path %>% 
  readRDS()

dir.create('output/plots/barplots/', recursive = T)

gene_ids = c('ENSG00000002586')
  # 'ENSG00000002586', 'ENSG00000002834', 'ENSG00000089486')


for (gene_id in gene_ids) { # [gene_id_i:gene_id_f]
  
  # tiff(paste0("output/plots/barplots/", gene_id, "_21vs21.tiff"), units="in", width=12, height=10, res=300)  
  

  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  par(cex.axis=0.8, mar=c(8, 4, 5, 2), units="in", width=12, height=10, res=300)
  
  norm_counts[gene_id, ] %>% 
    barplot(las = 2, 
            col = c(rep('gray', 21), 
                    rep('pink', 21)), 
            main = gene_id,
            ylim = c(0, max(norm_counts[gene_id, ])),
            ylab = 'Normalized counts'
    )
  
  data_bxplt = norm_counts[gene_id, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', 21), rep('APA_The', 21)))
  
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'Normalized counts', main = gene_id,
          ylim = c(0, max(norm_counts[gene_id, ])))
  
  print(gene_id)
  print(grep(gene_id, gene_ids))
  readline(prompt = "Press [enter] to continue")
  dev.off()
}
