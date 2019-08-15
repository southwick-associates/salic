context("License History Functions")
library(salic)
library(dplyr)

# these tests are mostly based on previous calculations (sample data)
# basically to allow refactoring of code with more assurance that nothing gets broken

# shared data & calculations
data(sale, lic, history)
sale_unranked <- left_join(sale, lic)
sale_ranked <- rank_sale(sale_unranked, first_month = TRUE)
history_calc <- sale_ranked %>%
    make_lic_history(2008:2019, carry_vars = c("month", "res"))


# New History Functions ---------------------------------------------------

test_that("make_history() produces expected result", {
    format_result <- function(x) {
        select(x, cust_id, year, duration_run) %>%
            arrange(cust_id, year)
    }
    x <- make_history(sale_ranked, 2008:2019) %>% format_result()
    y <- format_result(history)
    
    # start here, getting different number of rows
    # presumably the lapply filter, find out why
    expect_equal(x, y)
})

# Previous Functions ------------------------------------------------------



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

# Carry Forward Funcs -----------------------------------------------------
# these are closer to unit tests for make_lic_history

test_that("carry_duration() produces expected result", {
    yrs <- 2010:2015
    sale_ranked2 <- select(sale_ranked, cust_id, year, duration, month, res) %>%
        filter(year %in% yrs)
    format_same <- function(x) {
        select(x, cust_id, year) %>% arrange(cust_id, year) %>% data.frame()
    }
    # salic-estimated
    x <- split(sale_ranked2, sale_ranked2$year) %>% 
        carry_duration(yrs) %>% 
        bind_rows()
    
    # carry forward using an alternative (less efficient) method
    # this implementation feels a bit hacky
    y <- rename(sale_ranked2, ref_year = year) %>%
        merge(data.frame(year = yrs)) %>%
        mutate(duration = ref_year - year + duration) %>%
        filter(duration > 0, year > ref_year) %>%
        bind_rows(sale_ranked2) %>%
        distinct(cust_id, year) # resolve purchase/carry overlaps
    expect_equal(format_same(x), format_same(y))
})

test_that("carry_variables() produces expected result", {
    yrs <- 2008:2019
    format_same <- function(x) {
        select(x, cust_id, year, month, res) %>%
            arrange(cust_id, year) %>%
            data.frame()
    }
    z <- sale_ranked
    x <- split(sale_ranked, sale_ranked$year) %>%
        carry_duration(yrs) %>%
        carry_variables(yrs, c("month", "res")) %>%
        bind_rows()
    expect_equal(format_same(x), format_same(history))
})
