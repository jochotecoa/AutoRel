# wget https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE164541&format=file&file=GSE164541%5FANT%5Fcount%2Ecsv%2Egz
source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble'))

# download and decompress data  https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE178566&format=file&file=GSE178566%5FCount%5Ftable%2Etsv%2Egz

colcan = read.table('data/GSE164541_ANT_count.csv', header = T)

# listEnsemblArchives() %>% View()

colnames(colcan)[1] = 'ncbi_gene_id'

colcan_cts = colcan[-1] %>% 
  apply(2, round) %>% 
  as.data.frame() %>% 
  filterSamplesBySeqDepth()

rownames(colcan_cts) = unlist(colcan['ncbi_gene_id'])

cts_control = colcan_cts %>% 
  dplyr::select(contains('WT'))

cts_treatment = colcan_cts %>% 
  dplyr::select(contains('KO'))

cts = cbind.data.frame(cts_control, cts_treatment)

coldata = data.frame(row.names = colnames(cts), 
                     conditions = c(rep('WT', ncol(cts_control)), rep('KO', ncol(cts_treatment))),
                     batch = rep(1:6, 2))

rownames(coldata) %in% colnames(cts) %>% all() %>% stopifnot()
all(rownames(coldata) == colnames(cts)) %>% stopifnot()

dds = DESeqDataSetFromMatrix(countData = cts, 
                             colData = coldata, 
                             design = ~ batch + conditions)

dds <- DESeq(dds)

contrast = c("conditions","KO","WT")

method_dir = 'output/data/colcan'
dir.create(method_dir, recursive = T)

norm_counts = DESeq2::counts(object = dds, normalized = T)
res <- results(dds, contrast = c("conditions","KO","WT"))

degs = res[res$padj < 0.05,] %>% rownames()

for (deg in degs) {
  norm_counts[deg, ] %>% 
    barplot(las=2, col = c(rep('gray', 6), 
                           rep('pink', 6)))
  
  readline(prompt="Press [enter] to continue")
}

a = norm_counts[degs[2], ] %>% 
  as.data.frame() %>% 
  cbind(c(rep('WT', ncol(cts_control)), rep('KO', ncol(cts_treatment))))

colnames(a) = c('expr', 'cond')


boxplot(expr ~ cond, a)
