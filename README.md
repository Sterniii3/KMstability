# KMstability
Easy application of methods for reporting the stability of Kaplan-Meier estimates including time to censoring, observation time, observation time for those event-free and the stability interval proposed by Betensky (2015).

Install the package directly from [GitHub](https://github.com/) with:

```r
if(!require(devtools)) { install.packages("devtools"); require(devtools)} 
devtools::install_github("Sterniii3/KMstability")
````
and apply the functions to your data to generate Figures 1, 2, 3 and 4 as presented in Betensky (2015) and Tables 1 and 2 presentig derived metrics from it:
```r
library(KMstability)

data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
                  final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
                  event = rbinom(100, 1, 0.9))
                  
Figure1(data) # Kaplan–Meier estimate of survivor function for overall survival, with 95% confidence intervals and numbers at risk.
Figure2(data) # Upper and lower limits for Kaplan–Meier estimate as proposed by Betensky (2015).
Figure3(data) # Kaplan–Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C<X.
Figure4(data) # Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits.
Table1(data)  # Quantile summaries of KM estimate and proposed upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)
Table2(data)  # Quantile summaries of C, C|C<X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)
````
or access the user-friendly KMstability R Shiny App via: [https://web.imbi.uni-heidelberg.de/KMstability/](https://web.imbi.uni-heidelberg.de/KMstability/).



Reference:
Betensky, R. A. (2015). Measures of follow-up in time-to-event studies: Why provide them and what should they be?. Clinical Trials, 12(4), 403-408.
