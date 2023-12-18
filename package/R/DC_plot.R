#' DC_plot
#'
#' detailed graphical presentations of derived metrics of the alternative measure proposed by Betensky (2015)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return ggsurvplot object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
#' final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
#' event = rbinom(100, 1, 0.9))
#' DC_plot(data)
#'
#' @export
DC_plot <- function(data){

  DF <- calculate_stability(data)

  KM <- survival::survfit(survival::Surv(time, event) ~ strata,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # difference curve between the upper and lower limits and
  # the area under this curve, normalized by the maximum event time,
  # to range between 0 (complete stability) and 1 (complete instability)
  DF_norm <- survminer::surv_summary(KM, data = DF)
  # maximum observation time
  maximum_event_time <- ceiling(max(difftime(data$final_date,
                                             data$start_date)))
  DF_norm <- subset(DF_norm, time <= maximum_event_time)
  keep    <- as.numeric(names(which(table(DF_norm$time) == 3)))
  DF_norm <- subset(DF_norm, time %in% keep) # keep only observations where differences can be calculated
  time    <- subset(DF_norm, strata == "SI upper bound")$time
  diff_upper_lower <- subset(DF_norm,strata == "SI upper bound")$surv -
    subset(DF_norm, strata == "SI lower bound")$surv
  f       <- approxfun(time, diff_upper_lower, method = "constant")
  auc     <- integrate(f,
                       lower = min(time),
                       upper = max(time),
                       subdivisions = 10000L)
  norm_auc <- auc$value/as.numeric(maximum_event_time)

  # partial difference curves to indicate directional instability:
  # the difference between the upper limit and the Kaplan–Meier estimate and
  diff_upper_KM <- subset(DF_norm, strata == "SI upper bound")$surv -
    subset(DF_norm, strata == "KM estimate")$surv

  # the difference between the Kaplan–Meier estimate and the lower limit.
  diff_KM_lower <- subset(DF_norm, strata == "KM estimate")$surv -
    subset(DF_norm, strata == "SI lower bound")$surv

  # Figure 4. Difference curve between upper and lower limits of Kaplan–Meier
  # and partial difference curves between Kaplan–Meier and upper and lower limits
  # (compare Figure 4 in Betensky (2015))
  DF4 <- data.frame(time = c(time, time, time),
                    strata = c(rep("SI upper limit minus SI lower limit", length(time)),
                               rep("SI upper limit minus KM estmate", length(time)),
                               rep("KM estimate minus SI lower limit", length(time))),
                    Probability = c(diff_upper_lower,
                                    diff_upper_KM,
                                    diff_KM_lower))
  DF4$strata <- factor(DF4$strata, levels = c("SI upper limit minus SI lower limit",
                                              "SI upper limit minus KM estmate",
                                              "KM estimate minus SI lower limit"))

  plot4 <- ggplot2::ggplot(data = DF4,
                  ggplot2::aes(x = time,
                      y = Probability,
                      group = strata)) +
    # ggplot2::ggtitle("Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits") +
    ggplot2::geom_line(ggplot2::aes(linetype = strata)) +
    ggplot2::scale_linetype_manual(name="", values = c("solid",
                                              "dashed",
                                              "dotted"),
                          labels=c(paste0("SIU - SIL;\n normalized auc = ",
                                          round(norm_auc, 2 )),
                                   "SIU - KM",
                                   "KM - SIL")) +
    survminer::theme_survminer()

  return(plot4)
}
