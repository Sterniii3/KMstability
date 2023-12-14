#' FU_tab
#'
#' derived metrics of common measures of follow-up (i.e. time to censoring, observation time and observation time for those event-free)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#' @param pretty logical variable, if FALSE the output table is such that the values can easier be assessed for secondary analysis
#'
#' @return data.frame object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' FU_tab(data)
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
FU_tab <- function(data, pretty = TRUE){

  FU <- calculate_FU(data)

  KM_FU <- survival::survfit(survival::Surv(time, event) ~ strata,
                   type = "kaplan-meier",
                   conf.type = "log-log",
                   data = FU)

  # derived summary measures of measures of follow-up as measure of the stability of the KM estimates
  FU_tab <- as.data.frame(quantile(KM_FU, probs = c(0.75, 0.5, 0.25), conf.int = TRUE))

  names(FU_tab) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")
  row.names(FU_tab) <- c("C", "C|C<X", "T = min(X, C)")

  frame_pretty <- cbind(c("C", "C|C<X", "T = min(X, C)"),
                         t(t(paste0(FU_tab$`0.25quantile`, " (",
                                    FU_tab$`lower CI 0.25quantile`, ",",
                                    FU_tab$`upper CI 0.25quantile`, ")"))),
                         t(t(paste0(FU_tab$median, " (",
                                    FU_tab$`lower CI median`, ",",
                                    FU_tab$`upper CI median`, ")"))),
                         t(t(paste0(FU_tab$`0.75quantile`, " (",
                                    FU_tab$`lower CI 0.75quantile`, ",",
                                    FU_tab$`upper CI 0.75quantile`, ")"))))

  frame_pretty <- as.data.frame(frame_pretty)
  names(frame_pretty)<- c("","0.25", "0.5", "0.75")

  if(pretty == TRUE){

    FU_tab <- frame_pretty

  }

  return(FU_tab)

}
