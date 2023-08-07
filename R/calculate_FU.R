#' calculate_FU
#'
#' help function
#'
#' @param data data.frame containing variables named start_date, final_date and event.
#'
#' @return data.frame object
#'
#' @examples
#' data <- data.frame(start_date = as.Date(rep(0, 10), origin = "2022-01-01"), final_date = as.Date(c(120, 300, 500, 750, 290, 98, 167, 250, 734, 230), origin = "2022-01-01"), event = c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1))
#' calculate_FU(data)
#'
#' @export
calculate_FU <- function(data){

  ################################################################################
  # measures of follow-up as a measure of the stability of the KM estimator      #
  ################################################################################

  data$time <- difftime(data$final_date,
                        data$start_date)

  dfC <- subset(data, event == 0)

  FU = data.frame(strata = c(rep("C", dim(data)[1]), # time to censoring
                             rep("T = min(X, C)", dim(data)[1]), # observation time
                             rep("C|C<X", dim(dfC)[1])), # observation time among those who are censored
                  time = c(data$time,
                           data$time,
                           dfC$time),
                  event = c(1 - data$event,
                            rep(1, dim(data)[1]),
                            rep(1, dim(dfC)[1]))

  return(FU)
}
