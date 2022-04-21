source('scripts/functions/functions_JOA.R')

library(tidyverse)
forceLibrary(c('tidyverse', 'xlsx', 'UpSetR'))


go_terms <- read.csv(file = 'output/gorilla_results/21R/go_terms.csv', header = T)

go_terms %>% UpSetR::upset()
