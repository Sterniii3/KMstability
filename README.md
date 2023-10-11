# KMstability 

Easy application of methods for reporting the stability and precision of Kaplan-Meier estimates, as well as reporting (the median of) commonly used follow-up distributions such as time to censoring, observation time, observation time for       those event-free. 

## 1. Shiny application

Access the user-friendly KMstability R shiny application via: [https://web.imbi.uni-heidelberg.de/KMstability/](https://web.imbi.uni-heidelberg.de/KMstability/).
   
## 2. Package

Install the R package directly from [GitHub](https://github.com/) and load it by executing in the R console:

```r
if(!require(devtools)) { install.packages("devtools"); require(devtools)} 
devtools::install_github("Sterniii3/KMstability")

library(KMstability)
```
Apply the functions to your data to generate Figures 1, 2, 3 and 4 as presented in Betensky (2015) and Tables 1 and 2 presenting derived metrics from it:
```r
# generation of example data set
set.seed(41830619)
data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
                  final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
                  event = rbinom(100, 1, 0.9))
                  
Figure1(data) # Kaplan–Meier estimate of survivor function for overall survival, with 95% confidence intervals and numbers at risk.
Figure2(data) # Upper and lower limits for Kaplan–Meier estimate as proposed by Betensky (2015).
Figure3(data) # Kaplan–Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C<X.
Figure4(data) # Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits.
Table1(data)  # Quantile summaries of KM estimate and proposed upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI).
Table2(data)  # Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI).
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
                  
Figure1_group(data) 
Figure2_group(data) 
Figure3_group(data) 
Table1_group(data)  
Table2_group(data)  
````

Reference:

Betensky, R. A. (2015). Measures of follow-up in time-to-event studies: Why provide them and what should they be?. Clinical Trials, 12(4), 403-408.

Erdmann, S. & Betensky, R. A. (2023). How to report the stability and precision of Kaplan-Meier estimates as well as measures of follow-up in time-to-event studies: R tool KMstability. SoftwareX (submitted).
