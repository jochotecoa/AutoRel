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
  dplyr::select(contains('ConDMSO'))

cts_treatment = apap_cts %>% 
  dplyr::select(contains('APA_The'))

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
res <- results(dds, contrast = c("conditions","APA_The","ConDMSO"))


# Barplotting -------------------------------------------------------------


res2 = res[!is.na(res$padj), ]

degs = res2[res2$padj >= 0.1 & res2$padj < 0.2,] %>% 
  as.data.frame()
degs = degs[order(degs$padj), ] %>% 
  dplyr::filter(pvalue > 0.05)
degs %>% summary()
degs = degs %>% rownames()
# degs = res2[order(res2$padj, decreasing = T),] %>% rownames()

deg_i = grep("ENSG00000070061", degs)
deg_f = length(degs)

'ENSG00000138606		last'


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


# Putting final results toguether -----------------------------------------

manual_annot = read.csv('data/apap_hecatos_manual_significant.csv')

man_ann_res = subset.data.frame(x = res2, subset = rownames(res2) %in% manual_annot$ensembl_gene_id) %>% 
  as.data.frame()

manual_annot_2 = man_ann_res %>% 
  rownames_to_column('ensembl_gene_id') %>% 
  merge.data.frame(x = manual_annot, y = ., by = 'ensembl_gene_id') %>% 
  dplyr::select(ensembl_gene_id, significant, pvalue, padj) %>% 
  column_to_rownames('ensembl_gene_id')

manual_annot_2 = manual_annot_2[manual_annot$ensembl_gene_id, ]

big_padj_nondegs = res2 %>% 
  as.data.frame() %>% 
  dplyr::filter(padj >= 0.2) %>% 
  rownames()

non_degs_0.1 = res2 %>% 
  as.data.frame() %>% 
  dplyr::filter(padj >= 0.1, padj < 0.2, pvalue > 0.05)

non_degs_0.1 = non_degs_0.1[order(non_degs_0.1$padj), ]

which(rownames(non_degs_0.1) %in% rownames(manual_annot_2))
non_degs_0.1 = non_degs_0.1[1:grep("ENSG00000004487", rownames(non_degs_0.1)), ]

non_degs_0.1_0.05 = res2 %>% 
  as.data.frame() %>% 
  dplyr::filter(padj >= 0.1, padj < 0.2, pvalue < 0.05) 
 
  .[1:50, ] %>% 
  row.names()


manual_degs = data.frame(ensembl_gene_id = row.names(res), significance = NA)

na_non_degs = res[is.na(res$padj),] %>% 
  rownames()

manual_degs[manual_degs$ensembl_gene_id %in% na_non_degs, 'significance'] = F
manual_degs[manual_degs$ensembl_gene_id %in% big_padj_nondegs, 'significance'] = F


# Boxplotting -------------------------------------------------------------



degis = c('ENSG00000004487', 'ENSG00000005007')

for (degi in degis) {
  data_bxplt = norm_counts[degi, ] %>% 
    as.data.frame() %>% 
    cbind(c(rep('ConDMSO', ncol(cts_control)), rep('APA_The', ncol(cts_treatment))))
  data_bxplt = data.frame(ConDMSO = data_bxplt[grep('ConDMSO', rownames(data_bxplt)), 1], 
                          APA_The = data_bxplt[grep('APA_The', rownames(data_bxplt)), 1])
  boxplot(x = data_bxplt, ylab = 'normalized_counts', main = degi)
  readline(prompt = "Press [enter] to continue")
  
}


