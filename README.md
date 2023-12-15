# KMstability 

Easy application of methods for reporting the stability and precision of Kaplan-Meier estimates, as well as reporting (the median of) commonly used follow-up distributions such as time to censoring, observation time, observation time for       those event-free. 

## 1. application

Access the user-friendly KMstability R shiny application via: [https://web.imbi.uni-heidelberg.de/KMstability/](https://web.imbi.uni-heidelberg.de/KMstability/).
   
## 2. package

Install the R package directly from [GitHub](https://github.com/) and load it by executing in the R console:

```r
if(!require(devtools)) { install.packages("devtools"); require(devtools)} 
devtools::install_github(repo = "Sterniii3/KMstability", subdir = "/package") 

library(KMstability)
```
Apply the functions to your data to generate Figures 1, 2, 3 and 4 as presented in Betensky (2015) and Tables 1 and 2 presenting derived metrics from it:
```r
# generation of example data set
set.seed(41830619)
data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
                  final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
                  event = rbinom(100, 1, 0.9))
                  
KM_plot(data) # Kaplan–Meier estimate of survivor function for overall survival, with 95% confidence intervals and numbers at risk.
SI_plot(data) # Stability interval: upper and lower limits for Kaplan–Meier estimate as proposed by Betensky (2015).
FU_plot(data) # Kaplan–Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C<X.
DC_plot(data) # Difference curve between stability interval upper and lower limits and partial difference curves between Kaplan–Meier and SI upper and lower limits.
KM_SI_tab(data)  # Quantile summaries of KM estimate and proposed upper and lower bounds of the stability interval with associated 95% confidence intervals (lower CI, upper CI).
FU_tab(data, pretty = FALSE)  # Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI).
````

In a two group comparison it may also be useful to calculate the stability interval for the individual Kaplan Meier estimates, which can be done as follows:
```r
library(KMstability)
set.seed(41830619)
data <- data.frame(start_date = c(as.Date(rep(0, 200), origin = "2022-01-01"),
                                  as.Date(rep(0, 200), origin = "2022-01-01")),
                   final_date = c(as.Date(round(runif(200, 0, 250)), origin = "2022-01-01"),
                                  as.Date(round(runif(200, 0, 450)), origin = "2022-01-01")),
                   event = c(rbinom(200, 1, 0.9), rbinom(200, 1, 0.9)),
                   group = c(rep(0, 200), rep(1, 200)))
                  
KM_plot_group(data) 
SI_plot_group(data) 
FU_plot_group(data)
DC_plot_group(data)
KM_SI_tab_group(data)  
FU_tab_group(data)  
````

Reference:

Betensky, R. A. (2015). Measures of follow-up in time-to-event studies: Why provide them and what should they be?. Clinical Trials, 12(4), 403-408.

Erdmann, S. & Betensky, R. A. (2023). KMstability: R tools to report the stability and precision of Kaplan-Meier estimates as well as measures of follow-up in time-to-event studies. SoftwareX (in revision).
