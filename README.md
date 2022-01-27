# KMstability
Easy application of methods for reporting the stability of Kaplan-Meier estimates including time to censoring, observation time, observation time for those event-free and the stability interval proposed by Betensky (2015).

Install the package directly from [GitHub](https://github.com/) with:

```r
if(!require(devtools)) { install.packages("devtools"); require(devtools)} 
devtools::install_github("Sterniii3/KMstability")
````
and apply the functions to your data, as for example:
```r
library(KMstability)

data <- data.frame(start_date = as.Date(rep(0, 100), origin = "2022-01-01"),
                  final_date = as.Date(round(runif(100, 0, 250)), origin = "2022-01-01"),
                  event = rbinom(100, 1, 0.9))
Figure1(data)
Figure2(data)
Figure3(data)
Figure4(data)
Table1(data)
Table2(data)
````
or access the user-friendly KMstability R Shiny App via: [https://web.imbi.uni-heidelberg.de/KMstability/](https://web.imbi.uni-heidelberg.de/KMstability/).



Reference:
Betensky, R. A. (2015). Measures of follow-up in time-to-event studies: Why provide them and what should they be?. Clinical Trials, 12(4), 403-408.
