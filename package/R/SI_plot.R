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
#' Figure2(data)
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
