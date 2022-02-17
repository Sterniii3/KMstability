#' calculate_FU_group
#'
#' help function
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
#' calculate_FU_group(data)
#'
#' @export
calculate_FU_group <- function(data){

  data$time <- difftime(data$final_date,
                         data$start_date)

  ################################################################################
  # measures of follow-up as a measure of the stability of the KM estimator      #
  ################################################################################

  data0 <- subset(data, group == 0)
  data1 <- subset(data, group == 1)

  dfC <- subset(data0, event == 0)

  FU0 = data.frame(strata = c(rep("C", dim(data0)[1]), # time to censoring
                               rep("T = min(X, C)", dim(data0)[1]), # observation time
                               rep("C|C<X", dim(dfC)[1])), # time to censoring among those who are censored
                    time = c(data0$time,
                             data0$time,
                             dfC$time),
                    event = c((data0$event-1)*(-1),
                              rep(1, dim(data0)[1]),
                              (dfC$event-1)*(-1)))
  FU0$group <- 0

  dfC <- subset(data1, event == 0)

  FU1 = data.frame(strata = c(rep("C", dim(data1)[1]), # time to censoring
                               rep("T = min(X, C)", dim(data1)[1]), # observation time
                               rep("C|C<X", dim(dfC)[1])), # time to censoring among those who are censored
                    time = c(data1$time,
                             data1$time,
                             dfC$time),
                    event = c((data1$event-1)*(-1),
                              rep(1, dim(data1)[1]),
                              (dfC$event-1)*(-1)))
  FU1$group <- 1

  FU <- rbind(FU0, FU1)

  return(FU)
}
