# license history functions

# Making Tables -----------------------------------------------------------

#' Filter sales to 1 row per customer per year.
#'
#' The filter is based on one or more variables (rank_var); the row with the maximum 
#' value(s) of rank_var is selected. The default (and intended purpose) is to 
#' pick the maximum "duration" value per customer-year. This is a preliminary step
#' to \code{\link{make_lic_history}} which ensures that multi-year & lifetime sales
#' are always accounted for.
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
    if (!all(rank_var %in% colnames(sale))) {
        stop("All rank_var variable(s) (", paste(rank_var, collapse = ", "), 
             ") must be included in sale", call. = FALSE)
    }
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
#' (determined by highest duration value). This step ensures the earliest 
#' month value gets recorded in the license history table; enabling a simple
#' month filter of license history to display mid-year vs. full-year results.
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
#' # check month ranking - earliest month will always be picked
#' left_join(
#'     count(sale_ranked, month), 
#'     distinct(sale_unranked, cust_id, year, month) %>% count(month), 
#'     by = "month",
#'     suffix = c(".ranked", ".unranked")
#' )
join_first_month <- function(sale_ranked, sale_unranked) {
    if (!"month" %in% colnames(sale_unranked)) {
        stop("month variable must be included in sale_unranked", call. = FALSE)
    }
    first_month <- sale_unranked %>%
        arrange(.data$month) %>%
        group_by(.data$cust_id, .data$year) %>%
        slice(1L) %>%
        ungroup() %>%
        select(.data$cust_id, .data$year, .data$month)
    sale_ranked$month <- NULL
    left_join(sale_ranked, first_month, by = c("cust_id", "year"))
}

#' Make a License History table with 1 row per customer per year
#'
#' The license history table accounts for multi-year/lifetime licenses directly by including
#' a row for every year a license is held. The input data frame (sale_ranked) must 
#' have at least 3 columns (cust_id, year, duration) and should only have 1 record 
#' per customer per year (ensured by running \code{\link{rank_sale}} beforehand).
#' 
#' The output data frame includes the following variables:
#' \itemize{
#' \item \emph{cust_id}
#' \item \emph{year}
#' \item \emph{duration_run}: A duration variable that accounts for multi-year/lifetimes, 
#' also used in downstream R3 calculations.
#' \item \emph{carry_vars}: One or more optional variables to include, 
#' based on the corresponding argument (typically carry_vars = c("month", "res")).
#' For multi-year/lifetimes, carry_vars inherit values from the previous year.
#' }
#' 
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
#' history <- sale_ranked %>%
#'     make_lic_history(2007:2018, carry_vars = c("month", "res"))
#' 
#' # check a sample of several customers
#' check_history_samp(history)
make_lic_history <- function(sale_ranked, yrs, carry_vars = NULL) {
    
    # 1. initialize a list to store tracking table
    # looping over a list is (seemingly) simpler than iterating over data frame rows
    lic_history <- list()
    
    # 2. Initialize running duration for first year
    lic_history[[1]] <- sale_ranked %>%
        filter(.data$year == yrs[1]) %>%
        mutate(
            bought = TRUE, # bought a license this year
            duration_run = .data$duration # years remaining on privilege
        )
    
    # 3. Calculate running duration for subsequent years
    for (i in 2:length(yrs)) {
        lic_history[[i]] <- sale_ranked %>%
            filter(.data$year == yrs[i]) %>%
            
            # a. join current year (i) to previous year (i-1) to get lag_duration_run
            full_join(
                select(lic_history[[i-1]], .data$cust_id, lag_duration_run = .data$duration_run), 
                by = "cust_id"
            ) %>%
            
            # b. create current year variables
            # TODO - maybe create a separate function for this
            mutate(
                year = yrs[i],
                bought = ifelse(!is.na(.data$duration), TRUE, FALSE),
                
                duration_run = ifelse(
                    # duration_run coding depends on "bought" condition
                    # - if (bought in current year) we use current year's value
                    # - UNLESS it's smaller than the running duration (lag_duration_run - 1)
                    #   + this avoids the situation where (for example) a 1-year priv would replace
                    #   + a multi-year (or lifetime) license
                    #   + (i.e., it always favors the license with the longest remaining duration)
                    .data$bought, pmax(.data$duration, .data$lag_duration_run - 1, na.rm = TRUE),
                    
                    # otherwise we use the running duration
                    .data$lag_duration_run - 1
                )
            ) %>%
            # c. drop rows that don't represent current year privileges
            filter(.data$duration_run >= 1)
    }
    
    # 4. carry over variables that need to be populated for multi-year & lifetime (e.g., res)
    # uses dplyr-based tidyeval: http://dplyr.tidyverse.org/articles/programming.html
    if (!is.null(carry_vars)) {
        for (var in carry_vars) {
            # get most recent value for missing var
            for (i in 2:length(yrs)) {
                missing_vars <- lic_history[[i]] %>%
                    filter(is.na(.data[[var]])) %>%
                    left_join(
                        select(lic_history[[i-1]], .data$cust_id, lastvar = .data[[var]]),  
                        by = "cust_id"
                    ) %>%
                    mutate(!!var := .data$lastvar) %>%
                    select(-.data$lastvar)
                lic_history[[i]] <- lic_history[[i]] %>%
                    filter(!is.na(.data[[var]])) %>%
                    bind_rows(missing_vars)
            }
        }
    }
    
    # 5. Combine into data frame
    bind_rows(lic_history)[c("cust_id", "year", "duration_run", carry_vars)]
}

#' Identify R3 group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} and 
#' make the categorical variable R3 (1 = carried, 2 = renewed, 3 = reactivated, 4 = recruited), 
#' where "retained" consists of carried + renewed (hence R3: retain, reactivate, recruit).
#' 
#' @param lic_history license history data frame produced with \code{\link{make_lic_history}} 
#' @inheritParams make_lic_history
#' @param summary logical: if TRUE, include a textual summary for checking (by running 
#' \code{\link{check_identify_R3}})
#' @param check_vars logical: if TRUE, include output variables used in summary 
#' (lag_duration_run, yrs_since)
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' x <- history %>%
#'     select(-R3) %>%
#'     identify_R3(2008:2019, summary = TRUE)
#' 
#' # calculate recruitment rate
#' group_by(x, year, R3) %>% 
#'     summarise(n = n()) %>% 
#'     mutate(pct = n / sum(n)) %>%
#'     filter(R3 == 4, year != 2019)
identify_R3 <- function(lic_history, yrs, summary = FALSE, check_vars = FALSE) {
    # setup - last year held (to calcuate "yrs_since" for identifying recruits)
    out <- lic_history %>%
        arrange(.data$cust_id, .data$year) %>% # for correct lag ordering below
        group_by(.data$cust_id) %>% # to ensure lagged calculations are customer-specific
        mutate(lag_year = lag(.data$year), lag_duration_run = lag(.data$duration_run)) %>%
        ungroup() %>%
    
    # calculate R3
    mutate(
        yrs_since = .data$year - .data$lag_year,
        R3 = case_when(
            is.na(.data$yrs_since) | .data$yrs_since > 5 ~ 4L, # recruited
            .data$yrs_since == 1 & .data$lag_duration_run > 1 ~ 1L, # carried
            .data$yrs_since == 1 ~ 2L, # renewed
            TRUE ~ 3L # otherwise reactivated
        ),
        # 1st 5 yrs shouldn't be identified
        R3 = ifelse(.data$year > yrs[5], .data$R3, NA) 
    )
    if (summary) check_identify_R3(out, yrs) %>% data.frame() %>% print()
    if (!check_vars) out <- select(out, -.data$lag_duration_run, -.data$yrs_since)
    select(out, -.data$lag_year)
}

#' Identify lapse group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} 
#' and codes lapse: 0 = renews next year, 1 = lapses next year.
#' 
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
    # setup - next year held to easily identify lapse
    lic_history %>%
        arrange(.data$cust_id, .data$year) %>% # for correct lead ordering
        group_by(.data$cust_id) %>% # to ensure lead calculations are customer-specific
        mutate(lead_year = lead(.data$year)) %>%
        ungroup() %>%

        # calculate lapse
        mutate(
            lapse = case_when(
                .data$lead_year == (.data$year + 1) ~ 0L, # renewed
                TRUE ~ 1L # lapsed
            ),
            # ensure NA in final year
            lapse = ifelse(.data$year >= max(yrs), NA, .data$lapse) 
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
        count(.data$cust_id) %>%
        filter(.data$n >= buy_min, .data$n <= buy_max) %>%
        sample_n(n_samp) %>%
        left_join(lic_history, by = "cust_id") %>%
        select(-.data$n)
}

### START HERE - simply & specify it's use as an internal function ###

#' Check the output of \code{\link{identify_R3}}
#'
#' A test to insure R3 was correctly coded
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' select(history, -R3) %>%
#'     identify_R3(2008:2019, check_vars = TRUE) %>%
#'     check_identify_R3(2008:2019) %>%
#'     data.frame()
check_identify_R3 <- function(lic_history, yrs) {
    if (!"R3" %in% colnames(lic_history)) {
        warning("No R3 variable in lic_history", call. = FALSE)
        return(invisible())
    }
    if (all(is.na(lic_history$R3))) {
        warning("R3 is NA (missing) for all input records", call. = FALSE)
        return(invisible())
    }
    
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
