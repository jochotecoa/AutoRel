
# deseq2 pipeline ---------------------------------------------------------



# wget https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE164541&format=file&file=GSE164541%5FANT%5Fcount%2Ecsv%2Egz
source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble'))

# download and decompress data  https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE178566&format=file&file=GSE178566%5FCount%5Ftable%2Etsv%2Egz


cts = counts %>% 
  apply(2, as.integer) %>% 
  as.data.frame(row.names = rownames(counts)) 

dds = DESeqDataSetFromMatrix(countData = cts, 
                             colData = coldata, 
                             design = ~ Group)

dds <- DESeq(dds, quiet = T)

# contrast = c("conditions","APA_The","ConDMSO")
# 
# method_dir = 'output/data/apap'
# dir.create(method_dir, recursive = T)

norm_counts = DESeq2::counts(object = dds, normalized = T)
# dir.create('data/apap_hecatos')
# saveRDS(object = norm_counts, file = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos_21vs21.rds')


res <- results(dds)

# saveRDS(object = res, file = 'data/apap_hecatos/results_dds_deseq2_apap_hecatos_21vs21.rds')
# 
# saveRDS(object = dds, file = 'data/apap_hecatos/dds_deseq2_apap_hecatos_21vs21.rds')
