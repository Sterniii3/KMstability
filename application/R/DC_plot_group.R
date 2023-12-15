#' DC_plot_group
#'
#' detailed graphical presentations of derived metrics of the alternative measure proposed by Betensky (2015)
#'
#' @param data data.frame containing variables named start_date, final_date and event.
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
#' DC_plot_group(data)
#'
#' @export
DC_plot_group <- function(data){

  DF <- calculate_stability_group(data)

  KM <- survival::survfit(survival::Surv(time, event) ~ measure + group,
                type = "kaplan-meier",
                conf.type = "log-log",
                data = DF)

  # difference curve between the upper and lower limits and
  # the area under this curve, normalized by the maximum event time,
  # to range between 0 (complete stability) and 1 (complete instability)
  DF_norm <- survminer::surv_summary(KM, data = DF)

  DF_norm0 <- subset(DF_norm, group == 0)
  DF_norm1 <- subset(DF_norm, group == 1)

  # maximum event time
  maximum_event_time0 <- ceiling(max(DF_norm0$time))
  maximum_event_time1 <- ceiling(max(DF_norm1$time))

  DF_norm0 <- subset(DF_norm0, time <= maximum_event_time0)
  keep0    <- as.numeric(names(which(table(DF_norm0$time) == 3)))
  DF_norm0 <- subset(DF_norm0, time %in% keep0) # keep only observations where differences can be calculated
  time0    <- subset(DF_norm0, strata == "measure=SI upper bound, group=0")$time
  diff_upper_lower0 <- subset(DF_norm0,strata == "measure=SI upper bound, group=0")$surv -
    subset(DF_norm0, strata == "measure=SI lower bound, group=0")$surv
  f0       <- approxfun(time0, diff_upper_lower0, method = "constant")
  auc0     <- integrate(f0,
                       lower = min(time0),
                       upper = max(time0),
                       subdivisions = 10000L)
  norm_auc0 <- auc0$value/as.numeric(maximum_event_time0)

  DF_norm1 <- subset(DF_norm1, time <= maximum_event_time1)
  keep1    <- as.numeric(names(which(table(DF_norm1$time) == 3)))
  DF_norm1 <- subset(DF_norm1, time %in% keep1) # keep only observations where differences can be calculated
  time1    <- subset(DF_norm1, strata == "measure=SI upper bound, group=1")$time
  diff_upper_lower1 <- subset(DF_norm1,strata == "measure=SI upper bound, group=1")$surv -
    subset(DF_norm1, strata == "measure=SI lower bound, group=1")$surv
  f1       <- approxfun(time1, diff_upper_lower1, method = "constant")
  auc1     <- integrate(f1,
                        lower = min(time1),
                        upper = max(time1),
                        subdivisions = 10000L)
  norm_auc1 <- auc1$value/as.numeric(maximum_event_time1)

  # partial difference curves to indicate directional instability:
  # the difference between the upper limit and the Kaplan–Meier estimate and
  diff_upper_KM0 <- subset(DF_norm0, strata == "measure=SI upper bound, group=0")$surv -
    subset(DF_norm0, strata == "measure=KM estimate, group=0")$surv

  # the difference between the Kaplan–Meier estimate and the lower limit.
  diff_KM_lower0 <- subset(DF_norm0, strata == "measure=KM estimate, group=0")$surv -
    subset(DF_norm0, strata == "measure=SI lower bound, group=0")$surv

  diff_upper_KM1 <- subset(DF_norm1, strata == "measure=SI upper bound, group=1")$surv -
    subset(DF_norm1, strata == "measure=KM estimate, group=1")$surv

  # the difference between the Kaplan–Meier estimate and the lower limit.
  diff_KM_lower1 <- subset(DF_norm1, strata == "measure=KM estimate, group=1")$surv -
    subset(DF_norm1, strata == "measure=SI lower bound, group=1")$surv

  # Figure 4. Difference curve between upper and lower limits of Kaplan–Meier
  # and partial difference curves between Kaplan–Meier and upper and lower limits
  # (compare Figure 4 in Betensky (2015))
  DF4 <- data.frame(time = c(time0, time0, time0, time1, time1, time1),
                    group = c(rep(0, length(time0)),
                              rep(0, length(time0)),
                              rep(0, length(time0)),
                              rep(1, length(time1)),
                              rep(1, length(time1)),
                              rep(1, length(time1))),
                    strata = c(rep("0: SI upper limit minus SI lower limit", length(time0)),
                               rep("0: SI upper limit minus KM estmate", length(time0)),
                               rep("0: KM estimate minus SI lower limit", length(time0)),
                               rep("1: SI upper limit minus SI lower limit", length(time1)),
                               rep("1: SI upper limit minus KM estmate", length(time1)),
                               rep("1: KM estimate minus SI lower limit", length(time1))),
                    Probability = c(diff_upper_lower0,
                                    diff_upper_KM0,
                                    diff_KM_lower0,
                                    diff_upper_lower1,
                                    diff_upper_KM1,
                                    diff_KM_lower1))

  DF4$strata <- factor(DF4$strata, levels = c("0: SI upper limit minus SI lower limit",
                                              "0: SI upper limit minus KM estmate",
                                              "0: KM estimate minus SI lower limit",
                                              "1: SI upper limit minus SI lower limit",
                                              "1: SI upper limit minus KM estmate",
                                              "1: KM estimate minus SI lower limit"))
  DF4$group <- as.factor(DF4$group)


  plot4 <- ggplot2::ggplot(data = DF4,
                  ggplot2::aes(x = time,
                      y = Probability,
                      group = strata)) +
    # ggplot2::ggtitle("Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits") +
    ggplot2::geom_line(ggplot2::aes(linetype = strata, color = group)) +
    ggplot2::scale_linetype_manual(name="", values = c("solid",
                                                       "dashed",
                                                       "dotted",
                                                       "solid",
                                                       "dashed",
                                                       "dotted"),
                                   labels=c(paste0("0: SI upper limit - SI lower limit;\n normalized auc0 = ",
                                                   round(norm_auc0, 2 )),
                                            "0: SI upper limit - KM estmate",
                                            "0: KM estimate - SI lower limit",
                                            paste0("1: SI upper limit - SI lower limit;\n normalized auc1 = ",
                                                   round(norm_auc1, 2 )),
                                            "1: SI upper limit - KM estmate",
                                            "1: KM estimate - SI lower limit")) +
    survminer::theme_survminer()

  return(plot4)
}
