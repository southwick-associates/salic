context("License History Functions")
library(salic)
library(dplyr)

# shared data & calculations
yrs <- 2008:2019
data(sale, lic, history)
sale_unranked <- left_join(sale, lic)
sale_ranked <- rank_sale(sale_unranked, first_month = TRUE)

history_calc <- sale_ranked %>%
    make_history(yrs, carry_vars = c("month", "res"), show_diagnostics = TRUE) %>%
    arrange(cust_id, year)

# forward_vars() ----------------------------------------------------------

format_result <- function(x) select(x, cust_id, year, res)

test_that("forward_vars() result matches for purchase years (where !is.na(var))", {
    # this ensures that purchase data isn't modified unintentionally
    # note that missing values CAN be overwritten by previous values
    y <- filter(sale_ranked, !is.na(res))
    x <- semi_join(history_calc, y, by = c("cust_id", "year"))
    expect_equal(format_result(x), format_result(y))
})

test_that("forward_vars() result matches previous value for non-purchase years", {
    x <- history_calc %>%
        arrange(year) %>%
        group_by(cust_id) %>%
        mutate(res_lag = lag(res)) %>%
        ungroup() %>%
        anti_join(sale_ranked, by = c("cust_id", "year"))
    expect_true(all(x$res == x$res_lag))
})

# duration_run ------------------------------------------------------------

test_that("running duration not less than current year duration", {
    x <- history_calc
    expect_true(all((x$duration_run >= x$duration) | is.na(x$duration)))
})

test_that("running duration for non-purchase years determined by duration_lag", {
    x <- filter(history_calc, is.na(duration))
    expect_true(all(
        (x$duration_run == x$duration_run_lag - 1) | x$year == yrs[1]
    ))
})

# year_last ---------------------------------------------------------------

test_that("make_history() produced expected year_last", {
    # make sure a lagged year (in history) matches year_last    
    x <- history_calc
    y <- select(x, -year_last) %>%
        arrange(year) %>%
        group_by(cust_id) %>%
        mutate(year_last = lag(year)) %>%
        ungroup() %>%
        arrange(cust_id, year)
    expect_equal(x$year_last, y$year_last)
})

# lapse -------------------------------------------------------------------

test_that("make_history() produces expected lapse", {
    format_result <- function(x) select(x, cust_id, year, lapse)
    x <- history <- make_history(sale_ranked, 2008:2018)
    y <- arrange(x, year) %>%
        group_by(cust_id) %>%
        mutate(duration_lead = lead(duration_run), lead_year = lead(year)) %>%
        ungroup() %>%
        mutate(lapse = case_when(
            year == 2018 ~ NA_integer_,
            duration_lead >= 1 & lead_year == (year +1) ~ 0L,
            TRUE ~ 1L
        ))
    expect_equal(format_result(x), format_result(y))
})

# R3 ----------------------------------------------------------------------

history_r3 <- select(history_calc, cust_id, year) %>%
    arrange(year) %>%
    group_by(cust_id) %>%
    mutate(year_lag = lag(year)) %>%
    ungroup() %>%
    arrange(cust_id, year)

test_that("make_history() R3 - first 5 years are missing", {
    x <- filter(history_calc, year <= yrs[5])
    expect_true(all(is.na(x$R3)))
    
})

test_that("make_history() R3 - haven't bought in last 5 yrs are recruits", {
    x <- filter(history_calc, R3 == 4)
    y <- filter(history_r3, (year - year_lag) >= 6 | 
                    is.na(year_lag), year > yrs[5]) %>%
        mutate(R3 = 4)
    expect_equal(x$R3, y$R3)
})

test_that("make_history() R3 - didn't buy last year is reactivated", {
    x <- filter(history_calc, R3 == 3)
    y <- filter(history_r3, (year - year_lag) >= 2, (year - year_lag) <= 5,
                year > yrs[5]) %>%
        mutate(R3 = 3)
    expect_equal(x$R3, y$R3)
})

test_that("make_history() R3 - bought last year is carried/renewed", {
    x <- filter(history_calc, R3 %in% 1:2) %>% select(cust_id, year)
    y <- filter(history_r3, (year - year_lag) == 1, year > yrs[5]) %>%
        select(cust_id, year)
    expect_equal(x, y)
})

# make_history() ----------------------------------------------------------

test_that("make_history() produces expected result", {
    carry_vars <- c("res", "month")
    format_result <- function(x) {
        select(x, cust_id, year, duration_run, res, month) %>%
            arrange(cust_id, year)
    }
    # might do a compare to example data here as a catch-all
    # maybe not needed since individual units are covered
})

# rank_sale() ------------------------------------------------------

test_that("rank_sale() produces expected result", {
    # compare function output to a simple dplyr pipeline
    x <- rank_sale(sale_unranked, rank_var = "duration", 
                   grp_var = c("cust_id", "year")) %>%
        select(cust_id, year, duration)
    y <- arrange(sale_unranked, cust_id, year) %>%
        group_by(cust_id, year) %>%
        arrange(desc(duration)) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, duration)
    expect_equal(x, y)
    
    # 2 rank_var variables
    x <- rank_sale(sale_unranked, rank_var = c("duration", "res"),
                   grp_var = c("cust_id", "year")) %>%
        select(cust_id, year, duration, res)
    y <- arrange(sale_unranked, cust_id, year) %>%
        group_by(cust_id, year) %>%
        arrange(desc(duration), desc(res)) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, duration, res)
})

test_that("join_first_month() produces expected result", {
    x <- rank_sale(sale_unranked)
    x <- join_first_month(data.table::setDT(x), data.table::setDT(sale_unranked)) %>%
        as_tibble() %>%
        select(cust_id, year, month)
    y <- group_by(sale_unranked, cust_id, year) %>%
        arrange(month) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, month)
    expect_equal(x, y)
    
    x <- rank_sale(sale_unranked, first_month = TRUE) %>%
        select(cust_id, year, month)
    expect_equal(x, y)
})
