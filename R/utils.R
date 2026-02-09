#' Convert NA to Zero
#' @export
naToZero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

#' Convert Zero to NA
#' @export
zeroToNa <- function(x) {
  x[x == 0] <- NA
  return(x)
}

#' Add Pseudocount
#' @export
pseudocount <- function(x, addition = 1) {
  x <- x + addition
  return(x)
}

#' Calculate Interquartile Range
#' @export
iq <- function(x, na.rm = FALSE) {
  if (na.rm) { x <- stats::na.omit(x) }
  first_q <- as.numeric(stats::quantile(x, 0.25))
  third_q <- as.numeric(stats::quantile(x, 0.75))
  y <- third_q - first_q
  return(y)
}

#' Detect Outliers
#' @export
outlier <- function(x, y=NULL, na.rm = FALSE, onlyExtreme = FALSE) {
  outliers <- NULL
  if (na.rm) { x <- stats::na.omit(x) }
  q1 <- stats::quantile(x, 0.25)
  q3 <- stats::quantile(x, 0.75)
  
  if (!is.null(y)) {
    isOutlier <- y < q1 - 1.5*iq(x) | y > q3 + 1.5*iq(x)
    if (onlyExtreme) {
      isOutlier <- y < q1 - 3*iq(x) | y > q3 + 3*iq(x)
    }
    return(as.logical(isOutlier))
  }
  
  for (i in x) {
    isOutlier <- i < q1 - 1.5*iq(x) | i > q3 + 1.5*iq(x)
    if (onlyExtreme) {
      isOutlier <- i < q1 - 3*iq(x) | i > q3 + 3*iq(x)
    }
    if (isOutlier) { outliers <- c(outliers, i) }
  }
  return(outliers)
}

#' Shift Median of a Dataframe
#' @export
shift_median <- function(df, median_of_medians) {
  for (i in colnames(df)) {
    sample_vals <- df[, i]
    corr_factor <- median_of_medians - stats::median(sample_vals, na.rm = TRUE)
    df[, i] <- df[, i] + corr_factor
  }
  return(df)
}

#' Force Library (Legacy Wrapper)
#' @export
forceLibrary <- function(pkgs, ...) {
  # In a package context, we use Imports, but keep this for internal script compatibility.
  lapply(pkgs, library, character.only = TRUE)
  invisible()
}
