#' Simulate Clinical Data
#'
#' Simulates a simple dataset based on two parameters; the number of subjects and timepoints.
#' @param n Number of subjects for the simulated data
#' @param t Number of timepoints for each subject
#' @keywords cats
#' @export
#' @examples
#' simulate_sample_data(n = 10, t = 6)

simulate_clinical_data <- function(n,t) {

  require(dplyr)
  # Simulate Demographic / Baseline Data
  id_data <- data.frame(
    id = (1:n),
    age = round(rnorm(n = n, mean = 50, sd = 15), 0),
    gender = as.factor(rbinom(n = n, size = 1, prob = 0.5)),
    treatment = as.factor(rbinom(n = n, size = 1, prob = 0.5)),
    baseline = round(rnorm(n = n, 28, 10) ,2)
  )

  # Label values for the factor variables
  levels(id_data$gender) <- c("Female", "Male")
  levels(id_data$treatment) <- c("Active", "Placebo")

  # Simulate time based structure
  sim_data <- data.frame(
    id = rep(1:n, each = t),
    visit = rep(1:t, times = n)
  )

  # Merge on the demographic data
  sim_data <- left_join(sim_data, id_data, by = "id")

  # Create the response as a linear combination of variables
  # Then cap age at a minimum of 5
  sim_data <- sim_data %>%
    mutate(response = round(25 + 3.6 * as.numeric(gender) + 0.02 * baseline +
                              0.5 * (age - mean(age)) + 30 * as.numeric(treatment) +
                              2.25 * visit, 2),
           age = ifelse(age < 5, 5, age))


  # Add random noise to the response variable
  sim_data$response <- round(jitter(sim_data$response, amount = 10), 2)

  return(sim_data)
}
