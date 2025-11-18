#' Extract numeric variables from a dataset
#'
#' Filters a data frame or matrix to keep only numeric columns,
#' removes rows containing NA values, and returns a clean numeric matrix.
#'
#' @param data A data frame or matrix containing mixed variable types.
#'
#' @return A numeric matrix containing only numeric variables with
#'   rows containing missing values removed.
#'
#' @export
get_numeric_vars = function(data) {

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Data must be a data frame or numeric matrix.")
  }

  # Keep only numeric columns
  num_data <- data[, sapply(data, is.numeric), drop = FALSE]

  # Remove rows with NA
  num_data <- na.omit(num_data)
  num_data <- as.matrix(num_data)
  colnames(num_data) <- colnames(data)[sapply(data, is.numeric)]

  return(num_data)
}

