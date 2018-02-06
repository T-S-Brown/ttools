#' Get Correlations
#'
#' Given a dataset, calculate the correlation coefficient of each variable with a
#' chosen target variable. The number of observations in the data frame must match
#' the length of the target vector. Returns a data frame listing the feature names
#' and associated correlation coefficient.
#'
#' @param data Data Frame for which the correlations will be calculated
#' @param target OUtcome Vector to calculate pairwise correlations with
#' @keywords Correlation Quick Calculations
#' @export
#' @examples
#'
#'
#' get_correlations(data, data$target)
#'

get_correlations <- function(data, target) {

  # Initialise the index
  i <- 1

  # Initialise an empty vector to store the results
  results <- rep(0, length(colnames(data)))

  # Loop for each feature in the data frame
  for (feature in colnames(data)) {

    # Calculate Pairwise correlations. If unable to coerce the
    # feature to numeric, then will return NA for that feature
    results[i] <- tryCatch(cor(as.numeric(data[[feature]]),
                               target,
                               use = "pairwise.complete.obs"),
                           error = function(){return(NA)})

    # Add one to the index before repeating
    i <- i + 1

  }

  # Merge the results with the data column Names
  corr_data <- data.frame("feature" = colnames(data),
                          "coef" = results)


  return(corr_data)

}
