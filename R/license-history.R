# license history functions


# Making Tables -----------------------------------------------------------

#' Aggregate a Sale Table to ensure 1 row per customer per year.
#'
#' This function takes an input sale table and returns a filtered version with
#' a single row per customer-year. The filter is based on one or more variables 
#' identified in rank_var; the row with the maximum value(s) is selected. 
#' The default (and intended purpose) is to pick the highest "duration" value 
#' per customer-year.
#' 
#' @param sale data frame: Input sales data
#' @param rank_var character: name of variable(s) to use for ranking
#' @param grp_var character: name of variable(s) used for grouping
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale_ranked <- rank_sale(sale_unranked) %>%
#'     join_first_month(sale_unranked)
#'     
#' # check sale ranking - highest duration will always be picked
#' left_join(
#'     count(sale_ranked, duration), 
#'     distinct(sale_unranked, cust_id, year, duration) %>% count(duration), 
#'     by = "duration",
#'     suffix = c(".ranked", ".unranked")
#' )
rank_sale <- function(sale, rank_var = "duration", grp_var = c("cust_id", "year")) {
    # programming with dplyr, string input (which is a bit unusual):
    # - https://github.com/tidyverse/dplyr/issues/2662
    grp_var <- syms(grp_var)
    rank_var <- syms(rank_var)
    
    sale %>%
        # to ensure highest value of rank_var - sort ascending, pick last
        arrange(!!! grp_var, !!! rank_var) %>%
        group_by(!!! grp_var) %>%
        slice(n()) %>%
        ungroup()
}


#' Join earliest sale month by customer-year to ranked sale table.
#'
#' This function is only intended to be run following \code{\link{rank_sale}};
#' necessary since the sale ranking only keeps one row per cust_id-year,
#' which is determined by duration, not month. This step ensures the earliest 
#' month value gets recorded in the license history table.
#' 
#' @param sale_ranked data frame: Input ranked sales data
#' @param sale_unranked data frame: Input unranked sales data
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale_ranked <- rank_sale(sale_unranked) %>%
#'     join_first_month(sale_unranked)
#' 
#' # check month ranking
#' left_join(
#'     count(sale_ranked, month), 
#'     distinct(sale_unranked, cust_id, year, month) %>% count(month), 
#'     by = "month",
#'     suffix = c(".ranked", ".unranked")
#' )
join_first_month <- function(sale_ranked, sale_unranked) {
    if (is.null(sale_unranked$month)) {
        stop("'month' must be included in 'sale_unranked' data frame")
    }
    first_month <- sale_unranked %>%
        arrange(month) %>%
        group_by(cust_id, year) %>%
        slice(1L) %>%
        ungroup() %>%
        select(cust_id, year, month)
    sale_ranked$month <- NULL
    left_join(sale_ranked, first_month, by = c("cust_id", "year"))
}

#' Make a License History Table
#'
#' Output table is stored in a data frame with 1 row per customer-year
#'
#' The input data frame (sale) must have at least 3 columns
#' (cust_id, year, duration) and should only have 1 record per customer per year
#' (which can be insured by running \code{\link{rank_sale}}).
#'
#' Each data frame in the output data frame has at least 6 columns:
#' \itemize{
#' \item \emph{cust_id}: Customer ID
#' \item \emph{year}: Privilege year
#' \item \emph{duration}: Bought a license this year? (>=1: yes, ==0: no),
#' where duration = 1 refers to annual or short-term, 2 to 2 yr, 3 to 3 yr, ...,
#' duration = 99 is used to indicate lifetime licenses
#' \item \emph{duration_run}: Running duration, which is necessary for multi-year
#' and lifetime licenses
#' \item \emph{lag_duration_run}: Previous year duration_run. A temporary value
#' used in coding and for downstream checking
#' \item \emph{bought}: Bought a license this year? (TRUE, FALSE). If FALSE, that 
#' indicates a license carried over from a previous year
#' }
#' @param sale_ranked data frame: Sales table from which license history will be made
#' @param yrs numeric: Years in sales data (column 'year') from which
#' to create license history
#' @param carry_vars character: additional variables to carry over from previous year
#' (for multi-year and lifetime licenses).
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale_ranked <- rank_sale(sale_unranked) %>%
#'     join_first_month(sale_unranked)
#' history <- make_lic_history(sale_ranked, 2007:2018)
#' 
#' # check a sample of several customers
#' check_history_samp(history)
make_lic_history <- function(sale_ranked, yrs, carry_vars = NULL) {
    
    # 1. initialize a list to store tracking table
    # looping over a list is (seemingly) simpler than iterating over data frame rows
    lic_history <- list()
    
    # 2. Initialize running duration for first year
    lic_history[[1]] <- sale_ranked %>%
        filter(year == yrs[1]) %>%
        mutate(
            bought = TRUE, # bought a license this year
            duration_run = duration # years remaining on privilege
        )
    
    # 3. Calculate running duration for subsequent years
    for (i in 2:length(yrs)) {
        lic_history[[i]] <- sale_ranked %>%
            filter(year == yrs[i]) %>%
            # a. join current year (i) to previous year (i-1) to get lag_duration_run
            full_join(select(lic_history[[i-1]], cust_id, lag_duration_run = duration_run),  
                      by = "cust_id"
            ) %>%
            # b. create current year variables
            mutate(
                year = yrs[i],
                bought = ifelse(!is.na(duration), TRUE, FALSE),
                
                # make variable (duration_run), coding depends on "bought" condition
                duration_run = ifelse(
                    # if (bought in current year) we use current year's value
                    # UNLESS it's smaller than the running duration (lag_duration_run - 1)
                    # - this avoids the situation where (for example) a 1-year priv would replace
                    # - a multi-year (or lifetime) license
                    # - (i.e., it always favors the license with the longest remaining duration)
                    bought, pmax(duration, lag_duration_run - 1, na.rm = TRUE),
                    
                    # otherwise we use the running duration
                    lag_duration_run - 1)
            ) %>%
            # c. drop rows that don't represent current year privileges
            filter(duration_run >= 1)
    }
    
    # 4. carry over variables that need to be populated for multi-year & lifetime (e.g., res)
    # uses dplyr-based tidyeval: http://dplyr.tidyverse.org/articles/programming.html
    if (!is.null(carry_vars)) {
        for (var in carry_vars) {
            # get most recent value for missing var
            for (i in 2:length(yrs)) {
                missing_vars <- lic_history[[i]] %>%
                    filter(is.na(.data[[var]])) %>%
                    left_join(select(lic_history[[i-1]], cust_id, lastvar = .data[[var]]), 
                              by = "cust_id") %>%
                    mutate(!!var := lastvar) %>%
                    select(-lastvar)
                lic_history[[i]] <- lic_history[[i]] %>%
                    filter(!is.na(.data[[var]])) %>%
                    bind_rows(missing_vars)
            }
        }
    }
    
    # 5. Combine into data frame
    bind_rows(lic_history)
}

#' Identify R3 group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} and 
#' codes R3: 1 = carried, 2 = renewed, 3 = reactivated, 4 = recruited, 
#' where "retained" consists of carried + renewed (hence R3: retain, reactivate, recruit).
#' @param lic_history license history data frame produced with \code{\link{make_lic_history}} 
#' @inheritParams make_lic_history
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale <- rank_sale(sale_unranked) %>%
#'     join_first_month(sale_unranked)
#'     
#' yrs <- 2008:2019
#' history <- sale %>%
#'     make_lic_history(yrs) %>%
#'     identify_R3(yrs)
#' check_identify_R3(history, yrs)
#' 
#' # calculate recruitment rate
#' group_by(history, year, R3) %>% 
#' summarise(n = n()) %>% 
#'     mutate(pct = n / sum(n)) %>%
#'     filter(R3 == 4, year != 2019)
identify_R3 <- function(lic_history, yrs) {
    lic_history %>%
        arrange(cust_id, year) %>% # for correct lag ordering
        # grouping is to insure lagged calculations are customer-specific
        # (done in a separate step so the R3 coding below runs much faster)
        group_by(cust_id) %>%
        mutate(lag_year = lag(year)) %>%
        ungroup() %>%
        mutate(
            # make recruit designators to simplify R3 logic below
            # "new" haven't bought before, and "old" haven't bought in 5 years
            yrs_since = year - lag_year,
            new_recruit = ifelse(is.na(yrs_since), TRUE, FALSE),
            old_recruit = ifelse(yrs_since > 5 & !is.na(yrs_since), TRUE, FALSE),
            
            # CARRY (1): still have time left on their previous purchase
            R3 = ifelse(lag_duration_run > 1 & !is.na(lag_duration_run), 1L,
                        
            # RENEW (2): bought in current and had priv in previous year
            ifelse(bought & lag_duration_run == 1 & !is.na(lag_duration_run), 2L,
           
            # RECRUIT (4): coded before reactivate so that logic is simpler
            ifelse(new_recruit | old_recruit, 4L, 
          
            # REACTIVATE (3): bought this year bought not in previous
            3L))),

            # set to NA for first 5 years
            R3 = ifelse(year > yrs[5], R3, NA)
            
        ) %>%
        select(-new_recruit, -old_recruit, -lag_year)
}

#' Identify lapse group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} 
#' and codes lapse: 0 = renew next year, 1 = lapse next year.
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale <- rank_sale(sale_unranked) %>%
#'     join_first_month(sale_unranked)
#' 
#' yrs <- 2008:2019
#' history <- sale %>%
#'     make_lic_history(yrs) %>%
#'     # 2019 is incomplete (1st 6 months) & must be excluded from the function call
#'     identify_lapse(yrs[-length(yrs)])
#' check_identify_lapse(history)
#' 
#' # calculate churn rate
#' group_by(history, year) %>% 
#'     summarise(mean(lapse)) %>%
#'     mutate(year = year + 1)
identify_lapse <- function(lic_history, yrs) {
    lic_history %>%
        arrange(cust_id, year) %>% # for correct lead ordering
        group_by(cust_id) %>% # to insure lead calculations are customer-specific
        mutate( lead_year= lead(year) ) %>%
        ungroup() %>%
        mutate(
            # if didn't buy next year: lapsed, otherwise: renewed
            lapse = ifelse(lead_year == (year + 1) & !is.na(lead_year), 0L, 1L),
            # set to NA for final year (or any following)
            lapse = ifelse(year >= max(yrs), NA, lapse)
        )
}


# Checking & Summarizing --------------------------------------------------

#' Sample the output of \code{\link{make_lic_history}}
#'
#' Look at a sample of customers from license history table to check the 
#' year over year dynamics
#' 
#' @param n_samp numeric: number of customers to view
#' @param buy_min numeric: minimum number of license purchases for customers to include
#' @param buy_max numeric: maximum number of license purchases for customers to include
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See analysis function example:
#' ?make_lic_history
check_history_samp <- function(lic_history, n_samp = 3, buy_min = 3, buy_max = 8) {
    lic_history %>%
        count(cust_id) %>%
        filter(n >= buy_min, n <= buy_max) %>%
        sample_n(n_samp) %>%
        left_join(lic_history, by = "cust_id") %>%
        select(-n, -lag_duration_run)
}

#' Check the output of \code{\link{identify_R3}}
#'
#' A test to insure R3 was correctly coded
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See analysis function example:
#' ?identify_R3
check_identify_R3 <- function(lic_history, yrs) {
    # this function only produces an output if R3 has been identified
    # (which it might not have been if there aren't at least 5 years of data)
    
    # error - don't run if tidyr isn't installed
    if (!requireNamespace("tidyr", quietly = TRUE)) {
        stop("tidyr needed for this function to work. Please install it.",
             call. = FALSE)
    }
    if ("yrs_since" %in% names(lic_history)) {
        lic_history %>%
            filter(year > yrs[5]) %>%
            mutate(
                R3 = dplyr::recode(R3, `1` = "1-Carry", `2` = "2-Renew", 
                                   `3` = "3-Reactivate", `4` = "4-Recruit")
            ) %>%
            count(R3, yrs_since, lag_duration_run) %>% 
            tidyr::spread(R3, n)
    }
}

#' Check the output of \code{\link{identify_lapse}}
#'
#' A test to insure lapse was correctly coded
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See analysis function example:
#' ?identify_lapse
check_identify_lapse <- function(lic_history) {
    # error - don't run if tidyr isn't installed
    if (!requireNamespace("tidyr", quietly = TRUE)) {
        stop("tidyr needed for this function to work. Please install it.",
             call. = FALSE)
    }
    lic_history %>%
        mutate(
            yrs_till_next = ifelse(is.na(lead_year), "Never",  
                                   ifelse(lead_year - year == 1, "1", ">1"))
        ) %>%
        count(lapse, year, yrs_till_next) %>%
        tidyr::spread(year, n)
}
