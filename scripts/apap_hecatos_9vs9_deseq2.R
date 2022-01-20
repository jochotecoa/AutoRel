
# deseq2 pipeline ---------------------------------------------------------



# wget https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE164541&format=file&file=GSE164541%5FANT%5Fcount%2Ecsv%2Egz
source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble'))

# download and decompress data  https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE178566&format=file&file=GSE178566%5FCount%5Ftable%2Etsv%2Egz

apap = read.table('data/raw_count_hecatos_apap.txt', header = T)

# listEnsemblArchives() %>% View()

colnames(apap)[1] = 'ensembl_gene_id'

apap_cts = apap[-1] %>% 
  apply(2, round) %>% 
  as.data.frame() %>% 
  filterSamplesBySeqDepth()

rownames(apap_cts) = unlist(apap['ensembl_gene_id'])

cts_control = apap_cts %>% 
  dplyr::select(contains('ConDMSO')) %>% 
  dplyr::select(matches('002|008|024'))

cts_treatment = apap_cts %>% 
  dplyr::select(contains('APA_The')) %>% 
  dplyr::select(matches('002|008|024'))


cts = cbind.data.frame(cts_control, cts_treatment)

coldata = data.frame(row.names = colnames(cts), 
                     conditions = c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))

rownames(coldata) %in% colnames(cts) %>% all() %>% stopifnot()
all(rownames(coldata) == colnames(cts)) %>% stopifnot()

dds = DESeqDataSetFromMatrix(countData = cts, 
                             colData = coldata, 
                             design = ~ conditions)

dds <- DESeq(dds)

contrast = c("conditions","APA_The","ConDMSO")

method_dir = 'output/data/apap'
dir.create(method_dir, recursive = T)

norm_counts = DESeq2::counts(object = dds, normalized = T)
dir.create('data/apap_hecatos')
saveRDS(object = norm_counts, file = 'data/apap_hecatos/norm_counts_9vs9_deseq2_apap_hecatos.rds')


res <- results(dds, contrast = c("conditions","APA_The","ConDMSO"))

saveRDS(object = res, file = 'data/apap_hecatos/results_dds_9vs9_deseq2_apap_hecatos.rds')

# Barplotting -------------------------------------------------------------


res2 = res[!is.na(res$padj), ]

degs = res2 %>% 
  as.data.frame()
degs = degs[order(degs$padj), ]
degs %>% summary()
degs = degs %>% rownames()
# degs = res2[order(res2$padj, decreasing = T),] %>% rownames()

deg_i = grep("ENSG00000282885", degs)
deg_f = length(degs)


for (deg in degs) { # [deg_i:deg_f]
  
  padjv = res2[deg, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  median_contr = median(norm_counts[deg, contr_cols])
  quant1_treatm = quantile(norm_counts[deg, treatm_cols], 0.25)
  quant3_treatm = quantile(norm_counts[deg, treatm_cols], 0.75)
  
  medbel1st = median_contr < quant1_treatm
  medabo3rd = median_contr > quant3_treatm
  
  norm_counts[deg, ] %>% 
    barplot(las  =2, 
            col = c(rep('gray', ncol(cts_control)), 
                    rep('pink', ncol(cts_treatment))), 
            main = paste0(deg, '; padj = ', padjv, '; ', medbel1st, '; ', medabo3rd))
  print(deg)
  readline(prompt = "Press [enter] to continue")
}

for (deg in degs[deg_i:deg_f]) { # [deg_i:deg_f]
  
  padjv = res2[deg, 'padj'] 
  
  contr_cols = grep('ConDMSO', colnames(norm_counts))
  treatm_cols = grep('APA_The', colnames(norm_counts))
  
  median_contr = median(norm_counts[deg, contr_cols])
  quant1_treatm = quantile(norm_counts[deg, treatm_cols], 0.25)
  quant3_treatm = quantile(norm_counts[deg, treatm_cols], 0.75)
  
  medbel1st = median_contr < quant1_treatm
  medabo3rd = median_contr > quant3_treatm
  
  norm_counts[deg, ] %>% 
    barplot(las  =2, 
            col = c(rep('gray', ncol(cts_control)), 
                    rep('pink', ncol(cts_treatment))), 
            main = paste0(deg, '; padj = ', padjv, '; ', medbel1st, '; ', medabo3rd))
  print(deg)
  readline(prompt = "Press [enter] to continue")
}

# Boxplotting -------------------------------------------------------------



degis = c('ENSG00000000003')

for (degi in degis) {
  data_bxplt = norm_counts[degi, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = degi)
  readline(prompt = "Press [enter] to continue")
  
}




