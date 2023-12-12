#' FU_plot
#'
#' detailed graphical presentation of common measures of follow-up (i.e. time to censoring, observation time and observation time for those event-free)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return ggsurvplot object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' FU_plot(data)
#'
#' @export
FU_plot <- function(data){

  FU <- calculate_FU(data)

  KM_FU <- survival::survfit(survival::Surv(time, event) ~ strata,
                   type = "kaplan-meier",
                   conf.type = "log-log",
                   data = FU)

  # FU_plot
  plot3 <- survminer::ggsurvplot(KM_FU,
                      data = FU,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      censor = TRUE)

  return(plot3)

}
