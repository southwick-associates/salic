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
