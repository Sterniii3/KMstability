#' calculate_stability
#'
#' help function
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return data.frame object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 10), origin = "2022-01-01"), final_date = as.Date(c(120, 300, 500, 750, 290, 98, 167, 250, 734, 230), origin = "2022-01-01"), event = c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1))
#' calculate_stability(data)
#'
#' @export
calculate_stability <- function(data){
  ################################################################################
  # stability interval = lower and upper limit for KM (Betensky, 2015)           #
  ################################################################################

  # calculation of upper limit
  # by "setting all censored observations to a value larger than the maximum event
  # time (and retaining their status as censored)"
  #-------------------------------------------------------------------------------
  # maximum event time
  # df <- subset(data, event == 1)
  # maximum_event_time <- ceiling(max(difftime(df$final_date,
  #                                            df$start_date)))
  maximum_event_time <- ceiling(max(difftime(data$final_date,
                                             data$start_date)))
  # set all times of censored observations to the maximum event time
  data$timeupper <- difftime(data$final_date,
                             data$start_date)
  data$timeupper[data$event == 0] <- maximum_event_time

  # calculation of lower limit
  # by "coding all censored observations as events at the observed event times
  # immediately following their censoring times"
  #-------------------------------------------------------------------------------
  data$time <- difftime(data$final_date,
                        data$start_date)
  dfC <- subset(data, event == 0)
  dfE <- subset(data, event == 1)

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
  data <- rbind(dfC, dfE)
  # code all observations as events
  data$eventlower <- 1

  # data.frame containing information
  DF = data.frame(strata = c(rep("KM estimate", dim(data)[1]),
                             rep("SI lower bound", dim(data)[1]),
                             rep("SI upper bound", dim(data)[1])),
                  time = c(data$time,
                           data$timelower,
                           data$timeupper),
                  event = c(data$event,
                            data$eventlower,
                            data$event))
  DF$strata <- factor(DF$strata,
                      levels = c("SI upper bound",
                                 "KM estimate",
                                 "SI lower bound"))
  return(DF)
}
