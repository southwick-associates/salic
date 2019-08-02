# functions for recoding license data


#' Recode a state variable to deal with unusual values
#' 
#' The idea is to set any odd values to missing. It's a simple function that does
#' some minimal recoding
#' @param dat data frame: input data with a state variable
#' @param state_table data frame: input data holding valid abbreviations
#' @param oldvar character: name of state variable
#' @param newvar character: name of new state variable
#' @import dplyr
#' @family functions for recoding license data
#' @export
#' @examples
#' library(salic)
#' data(state_abbreviations)
#' data(cust)
#' x <- recode_state(cust, state_abbreviations)
#' dplyr::count(x, state_new, state)
recode_state <- function(dat, state_table, oldvar = "state", newvar = "state_new") {
    dat[[newvar]] <- gsub("0", "o", dat[[oldvar]]) %>% toupper()
    dat[[newvar]] <- ifelse(dat[[newvar]] %in% state_table$state, dat[[newvar]], NA)
    dat
}


#' Create a standardized month variable for sales table
#' 
#' The output month variable capture transaction year and month in one metric.
#' The month number is relative to the license year: 1 = Jan, 2 = Feb, etc, for
#' current/future years and 0 = Dec, -1 = Nov, ..., for previous years.
#' 
#' Assumes two variables are present in the sales table:
#' dot (transaction date in 'yyyy-mm-dd'), year (numeric license year)
#' @param sale data frame: Input sales table
#' @param month_range numeric: A vector of months allowed in the output. Defaults
#' to 0:12 since this is a common range of sales. Any months outside the range will be
#' set to either the lowest values (for those less than the range) or the
#' highest value (for those over). This prevents unusual sale months from appearing
#' in results.
#' @import dplyr
#' @return Returns a sales table with a standardized 'month' variable and
#' prints a validation output.
#' @family functions for recoding license data
#' @export
#' @examples
recode_month <- function(sale, month_range = 0:12) {
    
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


#' Create a standardized age category variable
#' 
#' Uses 'birth_year' (from cust) and 'year' (from sale) to identify
#' age for each license year
#' @param dat data frame: Input table
#' @param age_labs character: labels to use for age category
#' @param age_breaks numeric: breaks for age category passed to 
#' \code{\link[base]{cut}}
#' @param max_age numeric: maximum allowed age. Anything above will be set to missing.
#' @param suppress_check logical: If TRUE, does not print a coding summary
#' @import dplyr
#' @family functions for recoding license data
#' @export
#' @examples 
#' library(salic)
#' library(dplyr)
#' data(cust, sale, package = "salic")
#' sale2 <- left_join(sale, cust) %>% 
#'     recode_agecat()
recode_agecat <- function(
    dat, 
    age_labs = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),  
    age_breaks = c(-Inf, 17, 24, 34, 44, 54, 64, Inf), 
    max_age = 110,
    suppress_check = TRUE
) {
    # make variables
    dat <- dat %>% mutate(
        age_year = year - birth_year, 
        age_year = ifelse(age_year > 110, NA, age_year), 
        age = cut(age_year, breaks = age_breaks, labels = FALSE), 
        agecat = cut(age_year, breaks = age_breaks, labels = age_labs)
    )
    
    # check output
    if (!suppress_check) {
        cat("\nCategory Coding Summary:\n")
        count(dat, age_year, age, agecat) %>% 
            data.frame() %>% 
            print(row.names = FALSE)
    }
    dat
}
