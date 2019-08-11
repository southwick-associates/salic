context("Calculating dashboard metrics")
library(salic)
library(dplyr)

data(history)

test_that("est_part() returns simple count", {
    # overall
    x <- est_part(history, suppress_warning = TRUE, outvar = "var")
    y <- dplyr::count(history, year)
    expect_equal(x$var, y$n)
    
    # by variable
    x <- est_part(history, "R3", suppress_warning = TRUE, outvar = "var")
    y <- dplyr::filter(history, !is.na(R3))
    y <- dplyr::count(y, R3, year)
    expect_equal(x$var, y$n)
})

test_that("scaleup_part() output sum matches segment_total", {
    y <- est_part(history, suppress_warning = TRUE, outvar = "var")
    x <- est_part(history, "sex", suppress_warning = TRUE, outvar = "var") %>%
        scaleup_part(y, outvar = "var") %>% 
        group_by(year) %>%  
        summarise(var = sum(var))
    expect_equal(x$var, y$var)
})

test_that("est_churn returns simple mean", {
    # overall
    x <- est_churn(history, suppress_warning = TRUE, outvar = "var")
    y <- group_by(history, year) %>% 
        summarise(var = mean(lapse)) %>%
        mutate(year = year + 1) %>%
        filter(year != max(year))
    expect_equal(x$var, y$var)
})
