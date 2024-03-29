
<!-- README.md is generated from README.Rmd. Please edit that file -->
salic
=====

salic is an R package for preparing and summarizing agency data. In particular, it aims to simplify the production of national/regional dashboards by providing a set of functions that work across state agencies.

Installation
------------

-   First, install R: <https://www.r-project.org/>
-   Then install [salic](https://github.com/southwick-associates/salic/releases) and it's dependencies from the R console:

``` r
# Install dependencies
install.packages(c("dplyr", "data.table"))

# Install salic
f <- "https://github.com/southwick-associates/salic/releases/latest/download/salic"
install.packages(paste0(f, ".zip"), repos = NULL) # for Windows
install.packages(paste0(f, ".tgz"), repos = NULL) # for Mac
install.packages(paste0(f, ".tar.gz"), repos = NULL, type = "source") # or from source
```

Usage
-----

See [the vignette](https://southwick-associates.github.io/salic/articles/salic.html) for an introduction.

A template workflow for national/regional dashboards is available at <https://github.com/southwick-associates/dashboard-template>

### Example: fishing participation

Using `rank_sale()`, `make_history()`, and `est_part()`

``` r
library(dplyr)
library(salic)

# load data
data(lic, sale)

# build license history
history <- lic %>% 
    filter(type %in% c("fish", "combo")) %>% 
    inner_join(sale, by = "lic_id") %>% 
    rank_sale() %>% 
    make_history(yrs = 2008:2018)

# count total participants
est_part(history)
#> # A tibble: 11 x 3
#>    tot    year participants
#>    <chr> <int>        <int>
#>  1 All    2008         5874
#>  2 All    2009         7013
#>  3 All    2010         7201
#>  4 All    2011         7235
#>  5 All    2012         7646
#>  6 All    2013         7667
#>  7 All    2014         8380
#>  8 All    2015         8477
#>  9 All    2016         8575
#> 10 All    2017         8741
#> 11 All    2018         8556
```
