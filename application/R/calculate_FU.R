#' calculate_FU
#'
#' help function
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return data.frame object
#'
#' @details
#' The implementation of the methods is done as follows. For each individual
#' i the time of entry into the study t^{start}_{i}, the final recorded
#' date t^{final}_{i} and a status indicator s_i (taking the value 1 if
#' t^{final}_{i} is the date of the event and 0 if t^{final}_{i} is the
#' date of censoring) is observed. Then, letting y_i = t_i^{final} - t_i^{start},
#' the three commonly reported measures can be all calculated using the standard
#' Kaplan-Meier estimator, with the following adjustments:
#' \itemize{
#'    \item time to censoring: this is calculated via a Kaplan-Meier estimate based on
#' \{(y_i, 1- s_i), i=1, ..., n\},
#'    \item observation time: this is calculated via a Kaplan-Meier estimate based
#' on \{(y_i, 1), i=1, ..., n\},
#'    \item observation time for those event-free: this is calculated via a Kaplan-Meier
#' estimate based on \{(y_i, 1), i = 1,..., n such that s_i=0\}.
#' }
#'
#' @references
#' Erdmann, S. & Betensky, R. A. (2023). KMstability: R tools to report
#' the stability and precision of Kaplan-Meier estimates as well as measures
#' of follow-up in time-to-event studies. SoftwareX (in revision).
#'
#' Betensky, R. A. (2015). Measures of follow-up in time-to-event studies:
#' Why provide them and what should they be?. Clinical Trials, 12(4), 403-408.
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 10), origin = "2022-01-01"), final_date = as.Date(c(120, 300, 500, 750, 290, 98, 167, 250, 734, 230), origin = "2022-01-01"), event = c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1))
#' calculate_FU(data)
#'
#' @export
calculate_FU <- function(data){

  ################################################################################
  # measures of follow-up as a measure of the stability of the KM estimator      #
  ################################################################################

  data$time <- difftime(data$final_date,
                        data$start_date)

  dfC <- subset(data, event == 0)

  FU = data.frame(strata = c(rep("C", dim(data)[1]), # time to censoring
                             rep("T = min(X, C)", dim(data)[1]), # observation time
                             rep("C|C<X", dim(dfC)[1])), # observation time among those who are censored
                  time = c(data$time,
                           data$time,
                           dfC$time),
                  event = c(1 - data$event,
                            rep(1, dim(data)[1]),
                            rep(1, dim(dfC)[1])))

  return(FU)
}
