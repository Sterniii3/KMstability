#' KM_plot
#'
#' detailed graphical presentations of the entire distribution via adequate Kaplan-Meier plots including confidence intervals and detailed life tables
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return ggsurvplot object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' KM_plot(data)
#'
#' @export
KM_plot <- function(data, time_interval = FALSE){

  # Kaplan-Meier estimator of event free time
  if(time_interval == FALSE){
   KM <- survival::survfit(survival::Surv(difftime(final_date, start_date),
                     event) ~ 1,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = data)   
  }else{
  KM <- survival::survfit(survival::Surv(difftime,
                     event) ~ 1,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = data)
}
  # KM_plot. Kaplan–Meier estimate of survivor function for overall survival, X,
  # with 95% confidence intervals and numbers at risk
  # (compare Figure 1 in Betensky (2015))
  plot1 <- survminer::ggsurvplot(KM,
                      data = data,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      censor = TRUE,
                      title = "")

  return(plot1)

}
