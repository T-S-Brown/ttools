#' Change from Baseline
#'
#' Calculates the change in value of a variable over time - typically a baseline value.
#' Defaults references are to a common CDISC complient data structure.
#' @param data Dataset
#' @param var Variable of Interest
#' @param groupvar Variable to compute change within
#' @param timevar Variable that records timepoint of measurement
#' @param bltime Value of the baseline time
#' @param reftime Value of the reference time
#' @keywords wrangling
#' @export
#' @examples
#' cfb(data = data, var = AVAL, groupvar = USUBJID,
#' timevar = VISIT)
#'


cfb <- function(data, var, groupvar = USUBJID,
                timevar = VISIT, bltime = "Baseline", reftime = "Week 2") {

  # Refernce the variables
  key_var <- enquo(var)
  key_group <- enquo(groupvar)
  key_time <- enquo(timevar)


  new_data <- data %>%
    select(!!key_var, !!key_group, !!key_time)

  bl_data <- new_data %>%
    filter(!!key_time == bltime) %>%
    rename(baseline = !!key_var)

  ref_data <- new_data %>%
    filter(!!key_time == reftime) %>%
    rename(post_base = !!key_var,
           comp_time = !!key_time)

  data_out <- inner_join(bl_data, ref_data, by = "USUBJID") %>%
    mutate(cfb = post_base - baseline) %>%
    select(!!key_group, cfb)

  colnames(data_out)[2] <- paste("cfb_", rlang::get_expr(key_var), "_",
                                 gsub(" ", "", reftime), sep = "")

  return(data_out)


}
