# Introduction to Salic
-   [Overview](#overview)
-   [Standardized License Data](#standardized-license-data)
    -   [Formatting Rules](#formatting-rules)
    -   [Categorical Variables](#categorical-variables)
    -   [Customers (cust)](#customers-cust)
    -   [License Types (lic)](#license-types-lic)
    -   [License Sales (sale)](#license-sales-sale)
-   [License History](#license-history)
    -   [rank\_sale](#rank_sale)
    -   [make\_history](#make_history)
-   [Dashboard Metrics](#dashboard-metrics)
-   [Summary Output](#summary-output)

Overview
--------

Salic provides a set of functions to progress from license data to dashboard summaries. It also includes package dplyr on installation (see [the dplyr intro](https://dplyr.tidyverse.org/)). You'll want to load both packages when using salic:

``` r
library(salic)
library(dplyr)
```

This vignette walks through salic functionality using provided sample data (see `?data_salic` for details).

Standardized License Data
-------------------------

To facilitate a generalized workflow, salic is strict about data formatting, and some up-front effort is needed to standardize the relevant state-level data. That said, the dashboard data needs are fairly light (9 variables total in 3 tables).

Salic includes documentation about each table's formatting expectations (see `?cust`, `?sale`, `?lic` for details). The 3 tables should be related by 2 key columns:

![](relations.png)

### Formatting Rules

Salic helps you follow it's formatting rules with an included function (see `?data_check`). The function call is silent if all checks pass; warnings will be printed for every failed check.

``` r
data(cust, lic, sale) # load sample data
data_check(cust, lic, sale)

# introduce some rule-breaking changes
cust$cust_id[1:2] <- 1
lic$type <- NULL
data_check(cust, lic, sale)
#> Warning: cust: Primary key (cust_id) not unique: 29999 keys and 30000 rows
#> Warning: cust: Primary key (cust_id) is missing 1 value(s) present in the
#> sale table
#> Warning: lic: 1 Missing variable(s): type
```

### Categorical Variables

Codes bla di bla...

### Customers (cust)

Three customer columns are needed; "cust\_id" being the most important. The cust\_id should uniquely identify every row in the table (i.e., it's a primary key).

``` r
data(cust)
glimpse(cust)
#> Observations: 30,000
#> Variables: 3
#> $ cust_id    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
#> $ sex        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
#> $ birth_year <int> 1919, 1920, 1924, 1924, 1924, 1924, 1924, 1924, 192...

length(unique(cust$cust_id)) == nrow(cust)
#> [1] TRUE
```

Salic expects specific numeric codes for categorical variables. For example, "sex" must be 1 of 3 values for every customer (1, 2, or NA). You can view the corresponding labels using `salic::label_categories`.

``` r
count(cust, sex)
#> # A tibble: 3 x 2
#>     sex     n
#>   <int> <int>
#> 1     1 21965
#> 2     2  7498
#> 3    NA   537

label_categories(cust) %>% count(sex)
#> # A tibble: 3 x 2
#>   sex        n
#>   <fct>  <int>
#> 1 Male   21965
#> 2 Female  7498
#> 3 <NA>     537
```

To assist standardization, salic includes functions to check your data against the formatting rules (see `?data_check_table`). The function call is silent if all checks pass; warnings will be printed for every failed check.

``` r
data_check_cust(cust) 

# introduce some problems
tmp <- cust
tmp$cust_id[2] <- 1
tmp$birth_year <- NULL
data_check_cust(tmp)
#> Warning: cust: Primary key (cust_id) not unique: 29999 keys and 30000 rows
#> Warning: cust: 1 Missing variable(s): birth_year
```

### License Types (lic)

Three license type columns are needed. Similar to customers, license types should be uniquely identified using "lic\_id".

``` r
data(lic)
glimpse(lic)
#> Observations: 136
#> Variables: 3
#> $ lic_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16...
#> $ type     <chr> "combo", "combo", "combo", "combo", "combo", "combo",...
#> $ duration <int> 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 99, 99, 99, 99...

length(unique(lic$lic_id)) == nrow(lic)
#> [1] TRUE
```

Unlike the customer table, no missing values are allowed in the license types table:

``` r
tmp <- lic
tmp$type[1] <- NA
tmp$duration[1] <- NA
data_check_lic(tmp)
#> Warning: lic$type: Contains values that aren't allowed: NA
#> Warning: lic$duration: Contains values that aren't allowed: NA
```

The "type" column specifies whether a license provides a hunting privilege ("hunt"), a fishing privilege ("fish"), or both ("combo").

``` r
count(lic, type)
#> # A tibble: 3 x 2
#>   type      n
#>   <chr> <int>
#> 1 combo    18
#> 2 fish     36
#> 3 hunt     82
```

The "duration" column is necessary for building a license history. Specifically:

-   Multi-year license are identified with values &gt; 1 (e.g., 2 = 2-year, 3 = 3-year, etc.).
-   Lifetimes are identified using a special value: 99.
-   All other license types should = 1.

``` r
count(lic, duration)
#> # A tibble: 3 x 2
#>   duration     n
#>      <int> <int>
#> 1        1   102
#> 2        3    20
#> 3       99    14
```

### License Sales (sale)

The license sales table includes 5 variables. No primary key is needed; the table links to customers and license types using foreign keys.

``` r
data(sale)
glimpse(sale)
#> Observations: 148,706
#> Variables: 5
#> $ cust_id <int> 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, ...
#> $ lic_id  <int> 25, 30, 31, 60, 60, 22, 22, 55, 26, 26, 26, 26, 26, 26...
#> $ year    <int> 2016, 2011, 2011, 2009, 2011, 2008, 2009, 2009, 2008, ...
#> $ month   <int> 1, 11, 11, 10, 10, 5, 5, 5, 8, 8, 8, 8, 8, 8, 8, 9, 6,...
#> $ res     <int> 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
```

The "year" and "month" variables indicate the date of purchase (1 = Jan, 2 = Feb, etc.). The month variable is needed for building "mid-year" dashboards, which present a year-to-year view for the first six months of sales.

``` r
# all customers per year
filter(sale, year >= 2015) %>%
    distinct(cust_id, year) %>% 
    count(year)
#> # A tibble: 4 x 2
#>    year     n
#>   <int> <int>
#> 1  2015  7660
#> 2  2016  7568
#> 3  2017  7673
#> 4  2018  7281

# customers in the first six months
filter(sale, year >= 2015, month <= 6) %>%
    distinct(cust_id, year) %>%
    count(year)
#> # A tibble: 4 x 2
#>    year     n
#>   <int> <int>
#> 1  2015  3369
#> 2  2016  3461
#> 3  2017  3490
#> 4  2018  3220
```

You can also make use of "type" to focus on hunters or anglers specifically. Note, that since the data contain combination licenses, we can't use a `group_by` operation directly for this purpose.

``` r
filter(lic, type %in% c("hunt", "combo")) %>%
    inner_join(sale, by = "lic_id") %>%
    filter(year >= 2015) %>%
    distinct(cust_id, year) %>% 
    count(year)
#> # A tibble: 4 x 2
#>    year     n
#>   <int> <int>
#> 1  2015  2910
#> 2  2016  2868
#> 3  2017  3008
#> 4  2018  2925
```

The "res" variable identifies state residency for the customer. Storing this info at the transaction level provides additional flexibility since a person's residency status could conceivably change over time.

``` r
count(sale, res)
#> # A tibble: 2 x 2
#>     res      n
#>   <int>  <int>
#> 1     0  28365
#> 2     1 120341

label_categories(sale) %>% count(res)
#> # A tibble: 2 x 2
#>   res              n
#>   <fct>        <int>
#> 1 Resident    120341
#> 2 Nonresident  28365
```

License History
---------------

START HERE

Also think about whether the above is too much of a data dump...it might be good to show the workflow that is needed earlier. There is a risk that people will begin reading and just give up.

### rank\_sale

### make\_history

#### R3

#### Lapse

Dashboard Metrics
-----------------

-   filter by age
-   `label_categories()` & `recode_agecat()`
-   `est_part()`, `est_recruit()`, `est_churn()`

Summary Output
--------------
