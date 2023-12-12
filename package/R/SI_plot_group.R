#' SI_plot_group
#'
#' detailed graphical presentation of the alternative measure proposed by Betensky (2015) for group comparison
#'
#' @param data data.frame containing variables named start_date, final_date, event and group.
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
#' SI_plot_group(data)
#'
#' @export
SI_plot_group <- function(data){

  DF <- calculate_stability_group(data)

  KM_group <- survival::survfit(survival::Surv(time, event) ~ group + measure,
                      type = "kaplan-meier",
                      conf.type = "log-log",
                      data = DF)

  # Figure 2. Proposed upper and lower limits for Kaplan–Meier.
  SI_plot_group <- survminer::ggsurvplot(KM_group,
                            data = DF,
                            risk.table = TRUE,
                            #  conf.int = TRUE,
                            censor = TRUE,
                            title = "",
                            legend = "none",
                            palette = c("lightblue3", "blue", "lightblue2",
                                        "pink3", "red", "pink2"))

  return(SI_plot_group)
}
