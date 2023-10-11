#' Figure3_group
#'
#' detailed graphical presentation of common measures of follow-up (i.e. time to censoring, observation time and observation time for those event-free) in a group comparison
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
#' Figure3_group(data)
#'
#' @export
Figure3_group <- function(data){

  FU <- calculate_FU_group(data)

  KM_FU <- survival::survfit(survival::Surv(time, event) ~ strata + group,
                   type = "kaplan-meier",
                   conf.type = "log-log",
                   data = FU)

  # Figure 3
  plot3_group <- survminer::ggsurvplot(KM_FU,
                      data = FU,
                      risk.table = TRUE,
                      conf.int = TRUE,
                      censor = TRUE,
                      palette = c("darkblue", "blue",
                                  "black", "grey",
                                  "darkred", "red"))

  return(plot3_group)

}
