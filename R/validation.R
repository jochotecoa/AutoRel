#' Validate and Clean AutoRel Inputs
#' 
#' @param norm_counts Dataframe of normalized counts
#' @param coldata Dataframe of metadata
#' @param contrast_group Name of the contrast column
#' @param control_level Name of the control group
#' @export
validate_inputs <- function(norm_counts, coldata, contrast_group, control_level) {
  message("Performing pre-flight checks...")
  
  # 1. Check if contrast column exists
  if (!(contrast_group %in% colnames(coldata))) {
    stop(paste("ERROR: Contrast column '", contrast_group, "' not found in metadata.", sep=""))
  }
  
  # 2. Check if control level exists in that column
  lvls <- levels(as.factor(coldata[[contrast_group]]))
  if (!(control_level %in% lvls)) {
    stop(paste("ERROR: Control level '", control_level, "' not found in column '", contrast_group, 
               "'. Available levels: ", paste(lvls, collapse=", "), sep=""))
  }
  
  # 3. Align Sample IDs
  
  # Helper to get sample names from df (either row names or 1st column)
  get_best_samples <- function(df) {
    rn <- rownames(df)
    if (all(grepl("^[0-9]+$", rn)) && is.character(df[,1])) return(df[,1])
    return(rn)
  }
  
  # Clean counts gene names / sample columns
  # If 1st column is character and 2nd is numeric, 1st is likely gene names
  if (is.character(norm_counts[,1]) && is.numeric(norm_counts[,2])) {
    rownames(norm_counts) <- norm_counts[,1]
    norm_counts <- norm_counts[,-1]
  }
  
  # Brute force match finding:
  # Try to find which column/rownames in counts match which column/rownames in metadata
  find_overlap <- function(df1, df2) {
    c1 <- colnames(df1)
    r2 <- rownames(df2)
    col2 <- if(ncol(df2) >= 1) as.character(df2[,1]) else NULL
    
    # Direct match
    if (length(intersect(c1, r2)) > 0) return(list(type="colnames_vs_rownames", ids=intersect(c1, r2)))
    if (!is.null(col2) && length(intersect(c1, col2)) > 0) return(list(type="colnames_vs_col1", ids=intersect(c1, col2)))
    
    # make.names match (handles R converting hyphens to dots)
    c1_clean <- make.names(c1)
    if (length(intersect(c1, make.names(r2))) > 0) return(list(type="colnames_vs_rownames_clean", ids=r2[make.names(r2) %in% c1]))
    if (!is.null(col2) && length(intersect(c1, make.names(col2))) > 0) return(list(type="colnames_vs_col1_clean", ids=col2[make.names(col2) %in% c1]))
    
    return(NULL)
  }
  
  match_info <- find_overlap(norm_counts, coldata)
  
  if (is.null(match_info)) {
    # Debug info
    message("Counts colnames: ", paste(head(colnames(norm_counts)), collapse=", "))
    message("Metadata rownames: ", paste(head(rownames(coldata)), collapse=", "))
    stop("ERROR: No matching Sample IDs found. Please ensure your counts column names match your metadata Sample IDs.")
  }
  
  if (grepl("col1", match_info$type)) {
    rownames(coldata) <- coldata[,1]
  }
  
  common_samples <- match_info$ids
  
  # For the actual subsetting, we need to use the version found in norm_counts
  if (grepl("clean", match_info$type)) {
    # If we matched via make.names, we need to map them back
    # This is complex, let's just make everything make.names for consistency
    colnames(norm_counts) <- make.names(colnames(norm_counts))
    rownames(coldata) <- make.names(rownames(coldata))
    common_samples <- intersect(colnames(norm_counts), rownames(coldata))
  }
  
  if (length(common_samples) == 0) {
    stop("ERROR: No matching Sample IDs found between counts columns and metadata row names.")
  }
  
  if (length(common_samples) < ncol(norm_counts)) {
    message(paste("Note: Dropping", ncol(norm_counts) - length(common_samples), 
                  "samples found in counts but missing from metadata."))
  }
  
  # Subset and Reorder to match
  norm_counts <- norm_counts[, common_samples]
  coldata <- coldata[common_samples, , drop=FALSE]
  
  # 4. Check for NAs
  na_count <- sum(is.na(norm_counts))
  if (na_count > 0) {
    message(paste("Warning:", na_count, "missing values (NAs) found in counts. Converting to 0."))
    norm_counts[is.na(norm_counts)] <- 0
  }
  
  return(list(counts = norm_counts, metadata = coldata))
}
