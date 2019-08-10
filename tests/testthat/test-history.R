context("License History Functions")
library(salic)
library(dplyr)
data(sale, lic)

test_that("rank_sale() produces expected result", {
    # compare function output to a simple dplyr pipeline
    sale <- left_join(sale, lic, by = "lic_id")
    x <- rank_sale(sale, rank_var = "duration", grp_var = c("cust_id", "year")) %>%
        select(cust_id, year, duration)
    y <- arrange(sale, cust_id, year) %>%
        group_by(cust_id, year) %>%
        arrange(desc(duration)) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, duration)
    expect_equal(x, y)
    
    # 2 rank_var variables
    x <- rank_sale(sale, rank_var = c("duration", "res"),
                   grp_var = c("cust_id", "year")) %>%
        select(cust_id, year, duration, res)
    y <- arrange(sale, cust_id, year) %>%
        group_by(cust_id, year) %>%
        arrange(desc(duration), desc(res)) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, duration, res)
})

test_that("join_first_month() produces expected result", {
    sale_unranked <- left_join(sale, lic)
    sale_ranked <- rank_sale(sale_unranked)
    x <- join_first_month(sale_ranked, sale_unranked) %>%
        select(cust_id, year, month)
    y <- group_by(sale_unranked, cust_id, year) %>%
        arrange(month) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, month)
    expect_equal(x, y)
})