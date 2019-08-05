context("Calculating dashboard metrics")
library(salic)

test_that("est_part() returns simple count", {
    data(history)
    
    # overall
    x <- est_part(history, suppress_warning = TRUE)
    y <- dplyr::count(history, year)
    expect_equal(x$part, y$n)
    
    # by variable
    x <- est_part(history, "R3", suppress_warning = TRUE)
    y <- dplyr::filter(history, !is.na(R3))
    y <- dplyr::count(y, R3, year)
    expect_equal(x$part, y$n)
})
