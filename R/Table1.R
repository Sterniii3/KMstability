#' Table1
#'
#' derived metrics of the Kaplan-Meier estimate and alternative measure of the stability of the Kaplan-Meier estimate (Betensky, 2015)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return data.frame object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' Table1(data)
#'
#' @export
Table1 <- function(data){

  DF <- calculate_stability(data)

  KM <- survfit(Surv(time, event) ~ strata,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # metrics derived from the upper and lower limits
  #-------------------------------------------------------------------------------
  # quantile summaries of limits:
  Table_1 <- as.data.frame(quantile(KM, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))

  names(Table_1) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")
  row.names(Table_1) <- c("upper bound",
                          "KM estimate",
                          "lower bound")
  return(Table_1)
}
