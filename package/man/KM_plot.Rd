% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KM_plot.R
\name{KM_plot}
\alias{KM_plot}
\title{KM_plot}
\usage{
KM_plot(data)
}
\arguments{
\item{data}{data.frame containing variables named start_date, final_date and event.}
}
\value{
ggsurvplot object
}
\description{
detailed graphical presentations of the entire distribution via adequate Kaplan-Meier plots including confidence intervals and detailed life tables
}
\examples{
data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
event = rbinom(100, 1, 0.9))
KM_plot(data)

}
