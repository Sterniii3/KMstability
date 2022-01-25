#' Figure2
#'
#' detailed graphical presentation of the alternative measure proposed by Betensky (2015)
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
Figure2 <- function(data){

  DF <- calculate_stability(data)

  KM <- survfit(Surv(time, event) ~ strata,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # Figure 2. Proposed upper and lower limits for Kaplan–Meier estimate
  # (compare Figre 2 in Betensky (2015))
  plot2 <- ggsurvplot(KM,
                      data = DF,
                      risk.table = TRUE,
                      #  conf.int = TRUE,
                      censor = TRUE,
                      title = "",
                      legend = "none",
                      palette = c("gray50", "red", "gray51"))

  return(plot2)
}
