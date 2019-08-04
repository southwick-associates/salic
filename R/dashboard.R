# functions for building dashboard summary data


# Prepare Data ------------------------------------------------------------


#' Create age category based on year (of sale) and birth year
#' 
#' Uses 'birth_year' (from cust) and 'year' (from sale) to identify
#' age for each license year
#' 
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
#' library(dplyr)
#' data(cust, sale)
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

