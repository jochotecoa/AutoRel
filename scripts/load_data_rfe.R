# load the library
source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

registerDoMC(5)

# load the data
file_X = 'data/apap_hecatos/whole_data_preds.rds'
file_Y = 'data/apap_hecatos/whole_data_target.rds'

X = readRDS(file = file_X)
Y = readRDS(file = file_Y) %>% 
  unlist()

# dplyr::select(!contains('strand')) # including the strand column gives always an error, independently of the algorithm used

# sizes = seq(1, ncol(X), (ncol(X)-1)/5) %>% as.integer()
sizes = seq(1, ncol(X), as.integer(ncol(X)/5))

path_output = '../output_rfe_autosig'
if (!dir.exists(path_output)) {
  dir.create(path_output, recursive = T)
}
