#' Table2
#'
#' derived metrics of common measures of follow-up (i.e. time to censoring, observation time and observation time for those event-free)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return data.frame object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' Table2(data)
#'
#' @export
Table2 <- function(data){

  FU <- calculate_FU(data)

  KM_FU <- survfit(Surv(time, event) ~ strata,
                   type = "kaplan-meier",
                   conf.type = "log-log",
                   data = FU)

  # derived summary measures of measures of follow-up as measure of the stability of the KM estimates
  Table_2 <- as.data.frame(quantile(KM_FU, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))

  names(Table_2) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")
  row.names(Table_2) <- c("C", "C|C<X", "T = min(X, C)")

  return(Table_2)

}
