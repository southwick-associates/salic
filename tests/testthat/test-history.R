context("License History Functions")
library(salic)
library(dplyr)

# these tests are mostly based on previous calculations (sample data)
# basically to allow refactoring of code with more assurance that nothing gets broken

# shared data & calculations
data(sale, lic, history)
sale_unranked <- left_join(sale, lic)
sale_ranked <- rank_sale(sale_unranked)
history_calc <- sale_ranked %>%
    join_first_month(sale_unranked) %>%
    make_lic_history(2008:2019, carry_vars = c("month", "res"))

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
    x <- join_first_month(sale_ranked, sale_unranked) %>%
        select(cust_id, year, month)
    y <- group_by(sale_unranked, cust_id, year) %>%
        arrange(month) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, month)
    expect_equal(x, y)
})

test_that("make_lic_history() produces expected result", {
    x <- select(history_calc, cust_id, year, month, res)
    y <- select(history, cust_id, year, month, res)
    expect_equal(x, y)
})

test_that("identify_R3() produces expected result", {
    x <- history_calc %>%
        identify_R3(2008:2019) %>%
        select(cust_id, year, R3)
    y <- select(history, cust_id, year, R3)
    expect_equal(x, y)
})

test_that("identify_lapse() produces expected result", {
    x <- history_calc %>%
        identify_lapse(2008:2018) %>%
        select(cust_id, year, lapse)
    y <- select(history, cust_id, year, lapse)
    expect_equal(x, y)
})