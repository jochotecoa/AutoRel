# To run this algorithm, you need 2 files: norm_counts and res.

# This is the column name of your 'coldata' dataframe which you specified
# in DESeq2 to split between control and treatment

# contrast_group = NULL

library('progress')
library('caret')


# Set up your files -------------------------------------------------------


if (exists('dds')) {
  norm_counts = DESeq2::counts(object = dds, normalized = T)
  res <- results(dds)
  
} else {
  path_norm_counts = 'data/example_files/norm_counts.csv'
  path_res = 'data/example_files/res.csv'
}

contrast_group = 'Group'

output_path = 'output/example_files/'

# Generate the features and run the model ---------------------------------

source('scripts/autorrel/pipeline_autorrel.R')


# Save the results --------------------------------------------------------

if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = T)
}

write.csv(pred, file = paste0(pred, '/all_results.csv'))

relevant_genes = pred[pred$pred == 'relevant', ]
write.csv(pred, file = paste0(relevant_genes, '/relevant_genes.csv'))
