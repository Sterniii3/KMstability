#' KM_SI_tab
#'
#' derived metrics of the Kaplan-Meier estimate and alternative measure of the stability of the Kaplan-Meier estimate (Betensky, 2015)
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
#' KM_SI_tab(data)
#'
#'@details
#' The implementation of the method is done as follows. For each individual i
#' the time of entry into the study t^{start}_{i}, the final recorded date
#' t^{final}_{i} and a status indicator s_i (taking the value 1 if
#' t^{final}_{i} is the date of the event and 0 if t^{final}_{i} is
#' the date of censoring) is observed. Then, letting y_i = t_i^{final} -
#' t_i^{start}, the stability interval can be all calculated using the
#' standard Kaplan-Meier estimator, with the following adjustments.
#' Define the maximum observation time by
#' t^{max} = max_{i}(t^{final}_i - t^{start}_{i}). Then, the upper limit
#' of the stability interval can be calculated by setting y_i = t^{max}
#'   for all i with s_i =0. Therefore, all censored patients remain under
#' risk until the maximum observation time. Let t^{final}_{i^*} (with
#' s_{i^*}=1) be the event time immediately following the time of
#' censoring of observation i (i.e. t^{final}_{i} with s_i=0). Then,
#' the lower limit of the stability interval can be calculated by setting
#' y_i  = t^{final}_{i^*} - t^{start}_{i} for all i with s_i=0 and
#' then setting s_i=1 for all i. If the last observation is a censoring
#' event, i.e. there is an observation j for which there is no
#' t^{final}_{j^*} > t^{final}_{j} (with s_{j^*}=1 and s_j=0), s_j is
#' set to 1. By this definition, the lower limit can be understood as
#' "realistic" worst case scenario, in the sense, that "unrealistic" early
#' events do not occur. However, alternatives to this definition are
#' possible and the context should determine what is used.
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
KM_SI_tab <- function(data, pretty = TRUE, time_interval = FALSE){

  DF <- calculate_stability(data, time_interval)

  KM <- survival::survfit(survival::Surv(time, event) ~ strata,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # metrics derived from the upper and lower limits
  #-------------------------------------------------------------------------------
  # quantile summaries of limits:
  KM_SI_tab <- as.data.frame(quantile(KM, probs = c(0.75, 0.5, 0.25), conf.int = TRUE))

  names(KM_SI_tab) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")

  row.names(KM_SI_tab) <- c("SI upper bound",
                          "KM estimate",
                          "SI lower bound")

  frame_pretty <- cbind(c("SI upper bound", "KM estimate", "SI lower bound"),
                        t(t(paste0(KM_SI_tab$`0.25quantile`, " (",
                                   KM_SI_tab$`lower CI 0.25quantile`, ",",
                                   KM_SI_tab$`upper CI 0.25quantile`, ")"))),
                        t(t(paste0(KM_SI_tab$median, " (",
                                   KM_SI_tab$`lower CI median`, ",",
                                   KM_SI_tab$`upper CI median`, ")"))),
                        t(t(paste0(KM_SI_tab$`0.75quantile`, " (",
                                   KM_SI_tab$`lower CI 0.75quantile`, ",",
                                   KM_SI_tab$`upper CI 0.75quantile`, ")"))))

  frame_pretty <- as.data.frame(frame_pretty)
  names(frame_pretty)<- c("","0.25", "0.5", "0.75")

  if(pretty == TRUE){

    KM_SI_tab <- frame_pretty

  }

  return(KM_SI_tab)
}
