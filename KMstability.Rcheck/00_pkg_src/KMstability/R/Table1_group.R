#' Table1_group
#'
#' derived metrics of the Kaplan-Meier estimate and alternative measure of the stability of the Kaplan-Meier estimate (Betensky, 2015)
#'
#' @param data data.frame containing variables named start_date, final_date, event and group.
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
#' Table1_group(data)
#'
#' @export
Table1_group <- function(data){

  DF <- calculate_stability_group(data)

  KM <- survival::survfit(survival::Surv(time, event) ~ group + measure,
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

  return(Table_1)
}
