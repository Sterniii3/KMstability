# KMstability
Easy application of methods for reporting the stability of Kaplan-Meier estimates

Install the package directly from [GitHub](https://github.com/) with:

```r
if(!require(devtools)) { install.packages("devtools"); require(devtools)} 
devtools::install_github("Sterniii3/KMstability")
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
and access the KMstability App via [https://web.imbi.uni-heidelberg.de/KMstability/](https://web.imbi.uni-heidelberg.de/KMstability/).
