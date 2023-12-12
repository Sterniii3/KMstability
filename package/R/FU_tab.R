#' FU_tab
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
#' FU_tab(data)
#'
#' @export
FU_tab <- function(data, pretty = TRUE){

  FU <- calculate_FU(data)

  KM_FU <- survival::survfit(survival::Surv(time, event) ~ strata,
                   type = "kaplan-meier",
                   conf.type = "log-log",
                   data = FU)

  # derived summary measures of measures of follow-up as measure of the stability of the KM estimates
  FU_tab <- as.data.frame(quantile(KM_FU, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))

  names(FU_tab) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")
  row.names(FU_tab) <- c("C", "C|C<X", "T = min(X, C)")

  frame_pretty <- cbind(c("C", "C|C<X", "T = min(X, C)"),
                         t(t(paste0(FU_tab$`0.25quantile`, " (",
                                    FU_tab$`lower CI 0.25quantile`, ",",
                                    FU_tab$`upper CI 0.25quantile`, ")"))),
                         t(t(paste0(FU_tab$median, " (",
                                    FU_tab$`lower CI median`, ",",
                                    FU_tab$`upper CI median`, ")"))),
                         t(t(paste0(FU_tab$`0.75quantile`, " (",
                                    FU_tab$`lower CI 0.75quantile`, ",",
                                    FU_tab$`upper CI 0.75quantile`, ")"))))

  frame_pretty <- as.data.frame(frame_pretty)
  names(frame_pretty)<- c("","0.25", "0.5", "0.75")

  if(pretty == TRUE){

    FU_tab <- frame_pretty

  }

  return(FU_tab)

}
