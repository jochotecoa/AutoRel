source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble'))

  # download and decompress data  https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE178566&format=file&file=GSE178566%5FCount%5Ftable%2Etsv%2Egz

nkg2a = read.table('data/GSE178566_Count_table.tsv', header = T)

# listEnsemblArchives() %>% View()

colnames(nkg2a)[1] = 'ncbi_gene_id'

nkg2a_cts = nkg2a[-1] %>% 
  apply(2, round) %>% 
  as.data.frame() %>% 
  filterSamplesBySeqDepth()

rownames(nkg2a_cts) = unlist(nkg2a['ncbi_gene_id'])
  
cts_control = nkg2a_cts %>% 
  dplyr::select(contains('WT'))
  
cts_treatment = nkg2a_cts %>% 
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

method_dir = 'output/data/nkg2a'
dir.create(method_dir, recursive = T)

norm_counts = DESeq2::counts(object = dds, normalized = T)
res <- results(dds, contrast = c("conditions","KO","WT"))



a = norm_counts['3821', ] %>% 
  as.data.frame() %>% 
  cbind(c(rep('WT', ncol(cts_control)), rep('KO', ncol(cts_treatment))))

colnames(a) = c('expr', 'cond')


boxplot(expr ~ cond, a)
