#' KM_SI_tab
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
#' KM_SI_tab(data)
#'
#' @export
KM_SI_tab <- function(data, pretty = TRUE){

  DF <- calculate_stability(data)

  KM <- survival::survfit(survival::Surv(time, event) ~ strata,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # metrics derived from the upper and lower limits
  #-------------------------------------------------------------------------------
  # quantile summaries of limits:
  KM_SI_tab <- as.data.frame(quantile(KM, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))

  names(KM_SI_tab) <- c("0.25quantile",
                      "median",
                      "0.75quantile",
                      "lower CI 0.25quantile",
                      "lower CI median",
                      "lower CI 0.75quantile",
                      "upper CI 0.25quantile",
                      "upper CI median",
                      "upper CI 0.75quantile")

  row.names(KM_SI_tab) <- c("SI upper bound",
                          "KM estimate",
                          "SI lower bound")

  frame_pretty <- cbind(c("SI upper bound", "KM estimate", "SI lower bound"),
                        t(t(paste0(KM_SI_tab$`0.25quantile`, " (",
                                   KM_SI_tab$`lower CI 0.25quantile`, ",",
                                   KM_SI_tab$`upper CI 0.25quantile`, ")"))),
                        t(t(paste0(KM_SI_tab$median, " (",
                                   KM_SI_tab$`lower CI median`, ",",
                                   KM_SI_tab$`upper CI median`, ")"))),
                        t(t(paste0(KM_SI_tab$`0.75quantile`, " (",
                                   KM_SI_tab$`lower CI 0.75quantile`, ",",
                                   KM_SI_tab$`upper CI 0.75quantile`, ")"))))

  frame_pretty <- as.data.frame(frame_pretty)
  names(frame_pretty)<- c("","0.25", "0.5", "0.75")

  if(pretty == TRUE){

    KM_SI_tab <- frame_pretty

  }

  return(KM_SI_tab)
}
