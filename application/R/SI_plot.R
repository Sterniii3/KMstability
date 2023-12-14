#' SI_plot
#'
#' detailed graphical presentation of the stability interval (Betensky, 2015)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return ggsurvplot object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' SI_plot(data)
#'
#' @details
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
SI_plot <- function(data){

  DF <- calculate_stability(data)

  KM <- survival::survfit(survival::Surv(time, event) ~ strata,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # SI_plot. Proposed upper and lower limits for Kaplan–Meier estimate
  # (compare Figre 2 in Betensky (2015))
  plot2 <- survminer::ggsurvplot(KM,
                      data = DF,
                      risk.table = TRUE,
                      #  conf.int = TRUE,
                      censor = TRUE,
                      title = "",
                      legend = "none",
                      palette = c("gray50", "red", "gray51"))

  return(plot2)
}
