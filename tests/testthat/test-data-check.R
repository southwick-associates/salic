context("Checking Standardized Data")
library(salic)
library(dplyr)
data(cust, lic, sale)

test_that("data_primary_key() produces warning appropriately", {
    # should warn
    x <- bind_rows(lic, lic)
    expect_warning(
        data_primary_key(x, df_name = "lic", primary_key = "lic_id")
    )
    # should not warn
    expect_warning(
        data_primary_key(cust, df_name = "cust", primary_key = "cust_id"),
        regexp = NA
    )
    
})

test_that("data_required_vars() produces warning appropriately", {
    vars = c("cust_id", "lic_id", "year", "month", "res")
    
    # should warn
    x <- select(sale, "lic_id")
    expect_warning(
        data_required_vars(x, df_name = "sale", required_vars = vars)
    )
    # should not warn
    expect_warning(
        data_required_vars(sale, df_name = "sale", required_vars = vars),
        regexp = NA
    )
    
})

test_that("data_foreign_key() produces warning appropriately", {
    
    
    # should warn
    x <- filter(cust, cust_id > 5)
    expect_warning(
        data_foreign_key(df_foreign = sale, df_primary = x, key = "cust_id")
    )
    # should not warn
    expect_warning(
        data_foreign_key(df_foreign = sale, df_primary = cust, key = "cust_id"),
        regexp = NA
    )
    
})
