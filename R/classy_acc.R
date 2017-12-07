#' Classy Accuracy
#'
#' Quickly compute the accuracy of an algothim by indputing predicted and actual values of an outcome
#' @param pred Vector of predicted outcomes
#' @param real Outcomes the algortihm is attempting to predict
#' @keywords classification metric
#' @export
#' @examples
#'
#'
#' classy_acc(pred, real)
#'

classy_acc <- function(pred, real) {

  result_data <- data.frame(pred,
                            real)

  result_data <- dplyr::mutate(result_data,
                               outcome = dplyr::case_when(
                                 pred == real ~ 1,
                                 TRUE ~ 0
                               ))

  accuracy = sum(result_data$outcome) / length(result_data$outcome)

  return(accuracy)
}
