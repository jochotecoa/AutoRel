# /share/script/hecatos/juantxo/circRNA_project/
# Libraries and functions -------------------------------------------------

source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble'))
register(MulticoreParam(20))
getCts <- function(path, comp) {
  quant_file = list.files(path = path, pattern = comp, full.names = T) %>% 
      readRDS()

  quant_file_counts = quant_file  %>% 
    dplyr::select(contains('NumReads'))
  # Clean colnames
  colnames(quant_file_counts) = colnames(quant_file_counts) %>% 
    gsub(pattern = '/quant.sf', replacement = '')
  # Take away samples with low sequencing depth
  quant_file_counts = quant_file_counts %>% 
    dplyr::select(!matches('lowseqdepth'))
  # DESeq2 needs round numbers
  cts = apply(quant_file_counts, 2, round) %>% as.data.frame()
  
  rownames(cts) = quant_file$Name
  return(cts)
}
getSampleNames <- function(fullSampleName) {
  tablenames = fullSampleName %>% colnames() %>% strsplit('_') %>% 
    as.data.frame %>% t  
  tmpnt = ncol(tablenames) - 1
  smpl = ncol(tablenames) 
  stringnames = paste0(tablenames[, tmpnt], '_', tablenames[, smpl])
  return(stringnames)
}
getTimepoints <- function(fullSampleName) {
  tablenames = fullSampleName %>% colnames() %>% strsplit('_') %>% 
    as.data.frame %>% t  
  tmpnt = ncol(tablenames) - 1
  stringnames = tablenames[, tmpnt]
  return(stringnames)
}
getReplicates <- function(fullSampleName) {
  tablenames = fullSampleName %>% colnames() %>% strsplit('_') %>% 
    as.data.frame %>% t  
  rplct = ncol(tablenames)
  stringnames = tablenames[, rplct]
  return(stringnames)
}
rmDuplicatedColumns <- function(df) {
  df = df[!duplicated(as.list(df))]
  return(df)
}

# Input data --------------------------------------------------------------

stopifnot("No compound specified"= exists('comp'))
stopifnot("No path for the input data specified"= exists('path_data'))

# Parameters

setSizeFactorToOne = F # Default: FALSE
filtering = T # Default: TRUE
control_path = '/ngs-data/analysis/hecatos/juantxo/mRNA/quant_salmon/Homo_sapiens.GRCh38.cdna.ncrna.circbase/t0_controls/'
input_folder = path_data = 'data/salmon_circBase/'
method = path_data %>% strsplit(split = '/') %>% .[[1]] %>% .[2]
comp = 'AMI'
doses = c('The', 'Tox')

# Analysis ----------------------------------------------------------------


# if (grepl('DAU|DOX|EPI|IDA|CONDMSO', toupper(comp))) {
#   control = 'con_DF2'
# } else {
#   control = 'ConDMSO'
# }

control = 'T0_and_DF2'

quant_file = mergeFiles(path = control_path, files_patt = 'quant.sf', recursive = T, header = T)

quant_file_counts = quant_file  %>% 
  column_to_rownames('Name') %>% 
  dplyr::select(contains('NumReads'))
# Clean colnames
colnames(quant_file_counts) = colnames(quant_file_counts) %>% 
  gsub(pattern = '_quant/quant.sf', replacement = '') %>% 
  gsub(pattern = '^.*\\/\\/', replacement = '')
# Take away samples with low sequencing depth
quant_file_counts = quant_file_counts %>% 
  dplyr::select(!matches('lowseqdepth'))
# DESeq2 needs round numbers
cts_control = apply(quant_file_counts, 2, round) %>% as.data.frame()

cts_control = cts_control %>% 
  rmDuplicatedColumns() %>% 
  filterSamplesBySeqDepth()

cts_treatm = getCts(path = path_data, comp = comp) %>% 
  rmDuplicatedColumns() %>% 
  filterSamplesBySeqDepth()

cts_the = cts_treatm %>% 
  dplyr::select(contains('The'))

cts_tox = cts_treatm %>% 
  dplyr::select(contains('Tox'))


# condi = ncol(cts_control) == ncol(cts_the) & ncol(cts_control) == ncol(cts_tox)

# If number of samples is not equal
# for (bla in 1:2) {
#   if (!condi) {
#     x = getSampleNames(cts_control)
#     for (variable in c('cts_the', 'cts_tox')) {
#       variable_name = variable
#       variable = get(variable)
#       y = getSampleNames(variable)
#       if (all(x %in% y)) {
#         if (all(y %in% x)) {
#           next()
#         } else {
#           assign(x = variable_name, value = variable[, y %in% x])
#         }
#       } else {
#         cts_control = cts_control[, x %in% y]
#       } 
#       
#     }
#     
#   }
#   
# }

for (dose in doses) {
  cts_dose = get(paste0('cts_', tolower(dose)))
  
  # if (ncol(cts_control) > ncol(cts_dose)) {
  #   
  #   cts_control = cts_control[, 1:ncol(cts_dose), F]
  #   
  # } else if (ncol(cts_control) < ncol(cts_dose)) {
  #   
  #   cts_dose = cts_dose[, 1:ncol(cts_control), F]
  #   
  # }
  # 
  # stopifnot(ncol(cts_control) == ncol(cts_dose))
  
  cts = merge.data.frame(x = rownames_to_column(cts_control), y = rownames_to_column(cts_dose), 'rowname') %>% 
    column_to_rownames()
  
  # cts = cts[1:(nrow(cts)/100), , F]
  # Metadata ----------------------------------------------------------------
  
  source(file = 'scripts/deSeq2/metadata_t0_deseq2.R', local = T, echo = T)
  
  # DESeq2 pipeline ---------------------------------------------------------
  
  source(file = 'scripts/deSeq2/deseq2.R', local = T, echo = T)
  
  
  # Extract results from a DESeq analysis -----------------------------------
  source(file = 'scripts/deSeq2/extract_results_deseq2.R', local = T, echo = T)
  
}

