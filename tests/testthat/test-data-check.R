context("Checking Standardized Data")
library(salic)
library(dplyr)
data(cust, lic, sale)

# required_vars -----------------------------------------------------------

vars = c("cust_id", "lic_id", "year", "month", "res")
x <- select(sale, "lic_id")

test_that("data_required_vars() produces warning", {
    expect_warning(
        data_required_vars(x, df_name = "sale", required_vars = vars)
    )
})

test_that("data_required_vars() does not produce warning", {
    expect_warning(
        data_required_vars(sale, df_name = "sale", required_vars = vars),
        regexp = NA
    )
})

test_that("data_required_vars() produces an error", {
    expect_error(
        data_required_vars(x, df_name = "sale", required_vars = vars,
                           stop_error = TRUE)
    )
})


# Other Funcs -------------------------------------------------------------

test_that("data_primary_key() warnings", {
    # should warn
    x <- bind_rows(lic, lic) # not unique
    expect_warning(
        data_primary_key(x, df_name = "lic", primary_key = "lic_id")
    )
    x <- cust
    x$cust_id[1] <- NA # contains missings
    expect_warning(
        data_primary_key(x, df_name = "cust", primary_key = "cust_id")
    )
    # should not warn
    expect_warning(
        data_primary_key(cust, df_name = "cust", primary_key = "cust_id"),
        regexp = NA
    )
})

test_that("data_foreign_key() warnings", {
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

test_that("variable_allowed_values() warnings", {
    # should warn
    x <- c(13, sale$month)
    expect_warning(
        variable_allowed_values(x, "sale-month", 1:12)
    )
    x <- c(NA, "hi", -100, sale$month) # various values
    expect_warning(
        variable_allowed_values(x, "sale-month", 1:12)
    )
    # should not warn
    expect_warning(
        variable_allowed_values(sale$month, "sale-month", 1:12),
        regexp = NA
    )
    x <- c(NA, sale$month) # allowing NAs
    expect_warning(
        variable_allowed_values(x, "sale-month", c(1:12, NA)),
        regexp = NA
    )
})

test_that("data_allowed_values() warnings", {
    allowed_values <- list(
        type = c("hunt", "fish", "combo"), duration = 1:99
    )
    # should warn
    x <- lic
    x$duration[1] <- NA
    expect_warning(
        data_allowed_values(x, "lic", allowed_values)
    )
    x <- lic
    x$type[1] <- "hi there"
    expect_warning(
        data_allowed_values(x, "lic", allowed_values)
    )
    # should not warn
    allowed_values$type <- c(allowed_values$type, "hi there")
    expect_warning(
        data_allowed_values(x, "lic", allowed_values), 
        regexp = NA
    )
})

test_that("data_check_table() warnings", {
    # no warn
    expect_warning(data_check_cust(cust), regexp = NA)
    expect_warning(data_check_lic(lic), regexp = NA)
    expect_warning(data_check_sale(sale), regexp = NA)
    
    # warn
    c <- select(cust, -sex)
    l <- mutate(lic, lic_id = 1)
    s <- mutate(sale, year = NA)
    expect_warning(data_check_cust(c))
    expect_warning(data_check_lic(l))
    expect_warning(data_check_sale(s))
})

test_that("data_check() warnings", {
    # no warn
    expect_warning(data_check(cust, lic, sale), regexp = NA)
    
    # warn
    c <- select(cust, -birth_year)
    l <- mutate(lic, lic_id = NA)
    s <- mutate(sale, res = 2)
    expect_warning(data_check(c, l, s))
})