#' FU_tab_group
#'
#' derived metrics of common measures of follow-up (i.e. time to censoring, observation time and observation time for those event-free) for group comparison
#'
#' @param data data.frame containing variables named start_date, final_date, event and group..
#'
#' @return data.frame object
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
FU_tab_group <- function(data){

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

  return(FU_tab)

}
