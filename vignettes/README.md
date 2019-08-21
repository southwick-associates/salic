-   [Overview](#overview)
-   [Standardized License Data](#standardized-license-data)
    -   [Customers (cust)](#customers-cust)
    -   [License Types (lic)](#license-types-lic)
    -   [License Sales (sale)](#license-sales-sale)
-   [License History](#license-history)
-   [Dashboard Metrics](#dashboard-metrics)
    -   [Summary Output](#summary-output)
-   [Older - keep temporarily](#older---keep-temporarily)

Overview
--------

Salic helps you progress from license data to dashboard summaries. It also provides package dplyr on installation ([dplyr intro](https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html)). You'll want to load both packages:

``` r
library(salic)
library(dplyr)
```

This vignette walks through salic functionality using provided sample data (see `?data_salic` for details).

Standardized License Data
-------------------------

To provide a generalized workflow, salic is strict about the input license data format. That said, the dashboard data needs are fairly light: 3 tables related by 2 keys:

<br> ![](relations.png)

<br>

Salic provides formatting rules in the documentation (see `?cust`, `?lic`, `?sale`). An overview of each table is included below.

### Customers (cust)

Three customer columns are needed; "cust\_id" being the most important; it should uniquely identify every row in the table (i.e., it's a primary key).

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

Salic expects specific codes for categorical variables. For example, "sex" can include 3 values:

``` r
count(cust, sex)
#> # A tibble: 3 x 2
#>     sex     n
#>   <int> <int>
#> 1     1 21965
#> 2     2  7498
#> 3    NA   537

# you can check categorical codes using salic::label_categories()
label_categories(cust) %>% count(sex)
#> # A tibble: 3 x 2
#>   sex        n
#>   <fct>  <int>
#> 1 Male   21965
#> 2 Female  7498
#> 3 <NA>     537
```

Functions are provided to check your data against the formatting rules (see `?data_check_table` for details):

``` r
# silent if all checks pass
data_check_cust(cust) 

# prints a warning for every check that doesn't pass
tmp <- cust
tmp$cust_id[1:2] <- "dupkey"
tmp$birth_year <- NULL
data_check_cust(tmp)
#> Warning: cust: Primary key (cust_id) not unique: 29999 keys and 30000 rows
#> Warning: cust: 1 Missing variable(s): birth_year
```

### License Types (lic)

Three license type columns are needed. Similar to customers, license types should be uniquely identified using a "lic\_id" column.

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
#> Warning: lic$type: contains values that aren't allowed: NA
#> Warning: lic$duration: contains values that aren't allowed: NA
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

-   multi-year licenses hold values according to the number of years they last
-   Lifetimes are identified using a special value: 99
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

The license sale table includes 5 variables. No primary key is needed; the table links to customers and license types using foreign keys.

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

The "year" and "month" variables indicate the date of purchase. bla di bla...

The "res" variable, bla di bla...

License History
---------------

-   `rank_sale()` to do this...
    -   `join_first_month()` to do that...
-   `make_lic_history()`
    -   `identify_R3()`
    -   `identify_lapse()`

Need to also consider the key parameters for this process:

-   permission: specifically related to license types include (fish, hunt, combo)
-   timeframe: full-year or mid-year
-   years

Dashboard Metrics
-----------------

-   filter by age
-   `label_categories()` & `recode_agecat()`
-   `est_part()`, `est_recruit()`, `est_churn()`

### Summary Output

Older - keep temporarily
------------------------

Automatic labelling is also provided (via `label_categories`); this can be used to help check the coding:

``` r
label_categories(cust) %>% count(sex)
#> # A tibble: 3 x 2
#>   sex        n
#>   <fct>  <int>
#> 1 Male   21965
#> 2 Female  7498
#> 3 <NA>     537
```
