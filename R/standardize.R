# functions for recoding license data


#' Recode state abbreviations
#' 
#' This is a convenience function to help standardize odd-looking state abbreviations
#' based on a reference list of allowed abbreviations.
#' 
#' @param dat data frame: input data with a state variable
#' @param state_table data frame: input data holding valid abbreviations
#' @param oldvar character: name of state variable
#' @param newvar character: name of new state variable
#' @import dplyr
#' @family functions for standardizing state license data
#' @export
#' @examples 
#' data(state_abbreviations)
#' dat <- data.frame(state = c("M0", "mo", "Mo", "MR", "YY"))
#' recode_state(dat, state_abbreviations)
recode_state <- function(dat, state_table, oldvar = "state", newvar = "state_new") {
    dat[[newvar]] <- gsub("0", "o", dat[[oldvar]]) %>% toupper()
    dat[[newvar]] <- ifelse(dat[[newvar]] %in% state_table$state, dat[[newvar]], NA)
    dat
}


#' Standardize month (for Southwick Internal Use)
#' 
#' DON'T USE FOR NAT/REG PREPARATION (where year should be based on sale date)
#' 
#' The output month variable captures transaction year and month in one metric.
#' The month number is relative to the license year: 1 = Jan, 2 = Feb, etc, for
#' current/future years and 0 = Dec, -1 = Nov, ..., for previous years.
#' 
#' @param sale data frame: Input sales table with at lest 2 variables: dot
#' (transaction date in 'yyyy-mm-dd') and year (numeric license year)
#' @param month_range numeric: A vector of months allowed in the output. Defaults
#' to 0:12 since this is a common range of sales. Any months outside the range will be
#' set to either the lowest values (for those less than the range) or the
#' highest value (for those over). This prevents unusual sale months from appearing
#' in results.
#' @import dplyr
#' @return Returns a sales table with a standardized 'month' variable and
#' prints a validation output.
#' @family functions for standardizing state license data
#' @export
#' @examples 
#' # For Southwick analysts: see data preparation scripts
recode_month <- function(sale, month_range = 0:12) {
    # error - don't run if lubridate isn't installed
    if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("lubridate needed for this function to work. Please install it.",
             call. = FALSE)
    }
    # calculate standardized month
    sale <- sale %>% mutate(
        dot2 = lubridate::ymd(dot), 
        issue_month = lubridate::month(dot2), 
        issue_year = lubridate::year(dot2), 
        yr_diff = issue_year - year, 
        month = issue_month + yr_diff * 12
    )
    # enforce specific range (bottom or top coding as necessary)
    if (!is.null(month_range)) {
        sale <- sale %>% mutate(
            month = ifelse(month < min(month_range), min(month_range), month),
            month = ifelse(month > max(month_range), max(month_range), month)
        )
    }
    # check new month specification
    test <- count(sale, year, month, issue_year, issue_month)
    last_year <- max(test$year)
    cat("\nRecoding Summary:\n")
    filter(test, year == (last_year-1)) %>% data.frame() %>% print(row.names = FALSE) 
    
    # finalize by dropping temporary variables
    sale %>%
        select(-yr_diff, -issue_month, -issue_year, -dot2)
}
