
<!-- README.md is generated from README.Rmd. Please edit that file -->
salic
=====

The salic R package lays out a workflow to prepare and summarize agency data. In particular, it aims to simplify the production of national/regional dashboards with a set of functions that work across state agencies.

Installation
------------

-   First, install R: <https://www.r-project.org/>
-   Then install salic and it's R package dependencies:

``` r
# Install dependencies
install.packages(c("dplyr", "data.table"))

# Install salic from binary executables
install.packages("bla bla") # for Windows
install.packages("bla bla") # for Mac

# Alternatively, install salic from source
install.packages("devtools")
devtools::install_github("southwick-associates/salic")
```

Usage
-----

See [Introduction to salic](https://southwick-associates.github.io/salic/articles/salic.html) (`vignette("salic")` from the R console).

A template workflow for national/regional dashboards is available at <https://github.com/southwick-associates/dashboard-template>

### Example: fishing churn rate

Using `rank_sale()`, `make_history()`, `est_part()`, and `format_result()` from `?salic`.

``` r
library(dplyr)
library(salic)

# load standardized data
data(cust, lic, sale)

# build a license history
history <- lic %>% 
    filter(type %in% c("fish", "combo")) %>% 
    inner_join(sale, by = "lic_id") %>% 
    rank_sale() %>% 
    make_history(yrs = 2008:2018) %>% 
    left_join(cust, by = "cust_id")

# summarize participation for use in a dashboard
recode_agecat(history) %>%
    filter(!agecat %in% c("0-17", "65+")) %>%
    est_churn() %>%
    format_result(timeframe = "full-year", group = "fish")
#> # A tibble: 10 x 7
#>    timeframe group segment  year category metric value
#>    <chr>     <chr> <chr>   <dbl> <chr>    <chr>  <dbl>
#>  1 full-year fish  All      2009 All      churn  0.411
#>  2 full-year fish  All      2010 All      churn  0.442
#>  3 full-year fish  All      2011 All      churn  0.453
#>  4 full-year fish  All      2012 All      churn  0.432
#>  5 full-year fish  All      2013 All      churn  0.440
#>  6 full-year fish  All      2014 All      churn  0.422
#>  7 full-year fish  All      2015 All      churn  0.472
#>  8 full-year fish  All      2016 All      churn  0.493
#>  9 full-year fish  All      2017 All      churn  0.493
#> 10 full-year fish  All      2018 All      churn  0.501
```
