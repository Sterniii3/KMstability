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
#'@details
#' The implementation of the method is done as follows. For each individual i
#' the time of entry into the study t^{start}_{i}, the final recorded date
#' t^{final}_{i} and a status indicator s_i (taking the value 1 if
#' t^{final}_{i} is the date of the event and 0 if t^{final}_{i} is
#' the date of censoring) is observed. Then, letting y_i = t_i^{final} -
#' t_i^{start}, the stability interval can be all calculated using the
#' standard Kaplan-Meier estimator, with the following adjustments.
#' Define the maximum observation time by
#' t^{max} = max_{i}(t^{final}_i - t^{start}_{i}). Then, the upper limit
#' of the stability interval can be calculated by setting y_i = t^{max}
#'   for all i with s_i =0. Therefore, all censored patients remain under
#' risk until the maximum observation time. Let t^{final}_{i^*} (with
#' s_{i^*}=1) be the event time immediately following the time of
#' censoring of observation i (i.e. t^{final}_{i} with s_i=0). Then,
#' the lower limit of the stability interval can be calculated by setting
#' y_i  = t^{final}_{i^*} - t^{start}_{i} for all i with s_i=0 and
#' then setting s_i=1 for all i. If the last observation is a censoring
#' event, i.e. there is an observation j for which there is no
#' t^{final}_{j^*} > t^{final}_{j} (with s_{j^*}=1 and s_j=0), s_j is
#' set to 1. By this definition, the lower limit can be understood as
#' "realistic" worst case scenario, in the sense, that "unrealistic" early
#' events do not occur. However, alternatives to this definition are
#' possible and the context should determine what is used.
#'
#' @references
#' Erdmann, S. & Betensky, R. A. (2023). KMstability: R tools to report
#' the stability and precision of Kaplan-Meier estimates as well as measures
#' of follow-up in time-to-event studies. SoftwareX (in revision).
#'
#' Betensky, R. A. (2015). Measures of follow-up in time-to-event studies:
#' Why provide them and what should they be?. Clinical Trials, 12(4), 403-408.
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
  # maximum observation time
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
