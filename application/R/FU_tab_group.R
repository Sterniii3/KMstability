#' FU_tab_group
#'
#' derived metrics of common measures of follow-up (i.e. time to censoring, observation time and observation time for those event-free) for group comparison
#'
#' @param data data.frame containing variables named start_date, final_date, event and group..
#' @param pretty logical variable, if FALSE the output table is such that the values can easier be assessed for secondary analysis
#'
#' @return data.frame object
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
#' @examples
#' data <- data.frame(start_date = c(as.Date(rep(0, 200), origin = "2022-01-01"),
#' as.Date(rep(0, 200), origin = "2022-01-01")),
#' final_date = c(as.Date(round(runif(200, 0, 250)), origin = "2022-01-01"),
#'                as.Date(round(runif(200, 0, 450)), origin = "2022-01-01")),
#' event = c(rbinom(200, 1, 0.9), rbinom(200, 1, 0.9)),
#' group = c(rep(0, 200), rep(1, 200)))
#' FU_tab_group(data)
#'
#' @export
FU_tab_group <- function(data, pretty = TRUE){

  FU <- calculate_FU_group(data)

  KM_FU <- survival::survfit(survival::Surv(time, event) ~ group + strata,
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

  row.names(FU_tab) <- c("0: C", "0: C|C<X", "0: T = min(X, C)",
                         "1: C", "1: C|C<X", "1: T = min(X, C)")

  frame_pretty <- cbind(c("0: C", "0: C|C<X", "0: T = min(X, C)",
                          "1: C", "1: C|C<X", "1: T = min(X, C)"),
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
