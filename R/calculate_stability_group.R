#' calculate_stability_group
#'
#' help function to calculate the stability interval for the individual Kaplan Meier estimates in a group comparison
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
#' calculate_stability_group(data)
#'
#' @export
calculate_stability_group <- function(data){
  ################################################################################
  # stability interval = lower and upper limit for KM (Betensky, 2015)           #
  ################################################################################

  data$time <- difftime(data$final_date,
                         data$start_date)

  # calculation of upper limit
  # by "setting all censored observations to a value larger than the maximum event
  # time (and retaining their status as censored)"
  #-------------------------------------------------------------------------------
  # maximum event time
  data0 <- subset(data, group == 0)
  data1 <- subset(data, group == 1)
  # maximum_event_time <- ceiling(max(difftime(df$final_date,
  #                                            df$start_date)))
  maximum_event_time0 <- ceiling(max(difftime(data0$final_date,
                                              data0$start_date)))
  maximum_event_time1 <- ceiling(max(difftime(data1$final_date,
                                              data1$start_date)))
  # set all times of censored observations to the maximum event time
  data0$timeupper <- difftime(data0$final_date,
                             data0$start_date)
  data1$timeupper <- difftime(data1$final_date,
                              data1$start_date)
  data0$timeupper[data0$event == 0] <- maximum_event_time0
  data1$timeupper[data1$event == 0] <- maximum_event_time1

  # calculation of lower limit
  # by "coding all censored observations as events at the observed event times
  # immediately following their censoring times"
  #-------------------------------------------------------------------------------

  # group = 0
  dfC <- subset(data0, event == 0)
  dfE <- subset(data0, event == 1)

  for(i in 1:length(dfC$time)){

    # difference of censoring time of censor event i (Ci) and
    # observed event times following this event (E): m = E - Ci
    m <- ((dfE$time - dfC$time[i])[(dfE$time - dfC$time[i]) > 0])

    if(length(m) > 0){
      # replace time of censoring of event i with observed event time
      # immediately following this event (Ei): Ci + min(m) = Ci + Ei - Ci = Ei
      dfC$timelower[i] <- dfC$time[i] + min(m)
    }else{
      # if there is no event time following the censoring event i: no replacement?
      dfC$timelower[i] <- dfC$time[i]
    }

  }
  # keep times of events
  dfE$timelower <- NA
  dfE$timelower <- as.numeric(dfE$time)
  data0 <- rbind(dfC, dfE)
  # code all censored observations as events
  data0$eventlower <- 1

  # group = 1
  dfC <- subset(data1, event == 0)
  dfE <- subset(data1, event == 1)

  for(i in 1:length(dfC$time)){

    # difference of censoring time of censor event i (Ci) and
    # observed event times following this event (E): m = E - Ci
    m <- ((dfE$time - dfC$time[i])[(dfE$time - dfC$time[i]) > 0])

    if(length(m) > 0){
      # replace time of censoring of event i with observed event time
      # immediately following this event (Ei): Ci + min(m) = Ci + Ei - Ci = Ei
      dfC$timelower[i] <- dfC$time[i] + min(m)
    }else{
      # if there is no event time following the censoring event i: no replacement?
      dfC$timelower[i] <- dfC$time[i]
    }

  }
  # keep times of events
  dfE$timelower <- NA
  dfE$timelower <- as.numeric(dfE$time)
  data1 <- rbind(dfC, dfE)
  # code all censored observations as events
  data1$eventlower <- 1

  data = rbind(data0, data1)

  # data.frame containing information
  DF = data.frame(measure = c(rep("KM estimate", dim(data)[1]),
                              rep("lower bound", dim(data)[1]),
                              rep("upper bound", dim(data)[1])),
                  time = c(data$time,
                           data$timelower,
                           data$timeupper),
                  event = c(data$event,
                            data$eventlower,
                            data$event),
                  group = c(data$group,
                            data$group,
                            data$group),
                  LT = c(rep("KM estimate", dim(data)[1]),
                         rep("limit", dim(data)[1]),
                         rep("limit", dim(data)[1])))

  DF$measure <- factor(DF$measure, levels = c("upper bound",
                                              "KM estimate",
                                              "lower bound"))

  return(DF)
}
