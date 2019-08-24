# salic: An R package to prepare agency dashboard data

This package includes a set of functions for summarizing agency data, particularly for use in AFWA's national/regional dashboard effort. It's intended to simplify/standardize this process by providing a set of easy-to-use functions.

## Installation

Ensure a verson of R (>= 3.5.0) is installed: https://www.r-project.org/

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

## Usage

See [Introduction to salic](https://southwick-associates.github.io/salic/articles/salic.html) (`vignette("salic")` from the R console).

A template workflow for national/regional dashboards is available at https://github.com/southwick-associates/dashboard-template

### Example: fishing participants

Using `rank_sale()`, `make_history()`, `est_part()` from `?salic`.

``` r
library(dplyr)
library(salic)
data(cust, lic, sale)

history <- lic %>% 
    filter(type %in% c("fish", "combo")) %>% 
    inner_join(sale, by = "lic_id") %>% 
    rank_sale() %>% 
    make_history(2008:2018, carry_vars = "res") %>% 
    left_join(cust, by = "cust_id")
    
recode_agecat(history) %>%
    filter(!agecat %in% c("0-17", "65+")) %>%
    est_part()
```