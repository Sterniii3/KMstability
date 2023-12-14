#' KM_plot_group
#'
#' detailed graphical presentations of the entire distribution via adequate Kaplan-Meier plots including confidence intervals and detailed life tables for group comparison
#'
#' @param data data.frame containing variables named start_date, final_date, event and group.
#'
#' @return ggsurvplot object
#'
#' @examples
#' data <- data.frame(start_date = c(as.Date(rep(0, 200), origin = "2022-01-01"),
#' as.Date(rep(0, 200), origin = "2022-01-01")),
#' final_date = c(as.Date(round(runif(200, 0, 250)), origin = "2022-01-01"),
#'                as.Date(round(runif(200, 0, 450)), origin = "2022-01-01")),
#' event = c(rbinom(200, 1, 0.9), rbinom(200, 1, 0.9)),
#' group = c(rep(0, 200), rep(1, 200)))
#' KM_plot_group(data)
#'
#' @export
KM_plot_group <- function(data){

  # Kaplan-Meier estimator of event free time
  KM <- survival::survfit(survival::Surv(difftime(final_date, start_date),
                     event) ~ group,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = data)

  # Figure 1. Kaplan–Meier estimate of survivor function for overall survival, X,
  # with 95% confidence intervals and numbers at risk
  # (compare Figure 1 in Betensky (2015))
  KM_plot_group <- survminer::ggsurvplot(KM,
                      data = data,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      censor = TRUE,
                      title = "", risk.table.height= 0.4)

  return(KM_plot_group)

}
