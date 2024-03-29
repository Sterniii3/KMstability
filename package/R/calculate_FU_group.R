#' calculate_FU_group
#'
#' help function
#'
#' @param data data.frame containing variables named start_date, final_date, event and group.
#'
#' @return data.frame object
#'
#' @examples
#' data <- data.frame(start_date = c(as.Date(rep(0, 200), origin = "2022-01-01"),
#' as.Date(rep(0, 200), origin = "2022-01-01")),
#' final_date = c(as.Date(round(runif(200, 0, 250)), origin = "2022-01-01"),
#'                as.Date(round(runif(200, 0, 450)), origin = "2022-01-01")),
#' event = c(rbinom(200, 1, 0.9), rbinom(200, 1, 0.9)),
#' group = c(rep(0, 200), rep(1, 200)))
#' calculate_FU_group(data)
#'
#'@details
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
#' @export
calculate_FU_group <- function(data){

  data$time <- difftime(data$final_date,
                         data$start_date)

  ################################################################################
  # measures of follow-up as a measure of the stability of the KM estimator      #
  ################################################################################

  data0 <- subset(data, group == 0)
  data1 <- subset(data, group == 1)

  dfC <- subset(data0, event == 0)

  FU0 = data.frame(strata = c(rep("C", dim(data0)[1]), # time to censoring
                               rep("T = min(X, C)", dim(data0)[1]), # observation time
                               rep("C|C<X", dim(dfC)[1])), # observation time among those who are censored
                    time = c(data0$time,
                             data0$time,
                             dfC$time),
                    event = c(1 - data0$event,
                              rep(1, dim(data0)[1]),
                              rep(1, dim(dfC)[1])))
  FU0$group <- 0

  dfC <- subset(data1, event == 0)

  FU1 = data.frame(strata = c(rep("C", dim(data1)[1]), # time to censoring
                               rep("T = min(X, C)", dim(data1)[1]), # observation time
                               rep("C|C<X", dim(dfC)[1])), # observation time among those who are censored
                    time = c(data1$time,
                             data1$time,
                             dfC$time),
                    event = c(1 - data1$event,
                              rep(1, dim(data1)[1]),
                              rep(1, dim(dfC)[1])))
  FU1$group <- 1

  FU <- rbind(FU0, FU1)

  return(FU)
}
