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
        arrange(!!! rank_var) %>%
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
    sale_unranked %>%
        arrange(.data$month) %>%
        group_by(.data$cust_id, .data$year) %>%
        slice(1L) %>%
        ungroup() %>%
        select(.data$cust_id, .data$year, .data$month) %>%
        left_join(
            select(sale_ranked, -.data$month),
            by = c("cust_id", "year")
        )
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
#' Development Note: This function is largely a wrapper for \code{\link{carry_duration}} 
#' and \code{\link{carry_variables}}
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
    x <- sale_ranked[c("cust_id", "year", "duration", carry_vars)] %>%
        filter(.data$year %in% yrs)
    split(x, x$year) %>%
        carry_duration(yrs) %>%
        carry_variables(yrs, carry_vars) %>%
        bind_rows() %>% 
        rename(duration_run = duration)
}

#' Internal Function: Carry specified variables forward
#' 
#' This function is intended to be called from \code{\link{make_lic_history}}.
#' 
#' @inheritParams make_lic_history
#' @inheritParams carry_duration
#' @import dplyr
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' 
#' yrs <- 2008:2019
#' carry_vars <- c("month", "res")
#' 
#' sale_unranked <- left_join(lic, sale)
#' sale_ranked <- rank_sale(sale_unranked) 
#' sale_split <- sale_ranked[c("cust_id", "year", "duration", carry_vars)] %>%
#'     split(sale_ranked$year) %>%
#'     carry_duration(yrs)
#'     
#' carry_variables(sale_split, yrs, c("month", "res"))
carry_variables <- function(sale_split, yrs, carry_vars) {
    if (is.null(carry_vars)) {
        return(sale_split)
    }
    if (any(!carry_vars %in% colnames(sale_split[[1]]))) {
        stop("All carry_vars (", paste(carry_vars, sep = ", "),
             ") needed for make_lic_history()", call. = FALSE)
    }
    # replace missings (of var) with value from previous year (if available)
    carry_one_variable <- function(sale_split, yrs, var) {
        for (i in 2L:length(yrs)) {
            sale_split[[i]] <- filter(sale_split[[i]], is.na(.data[[var]])) %>%
                left_join(
                    select(sale_split[[i-1]], .data$cust_id, lastvar = .data[[var]]),  
                    by = "cust_id"
                ) %>%
                mutate(!! var := .data$lastvar) %>%
                select(-.data$lastvar) %>%
                bind_rows(filter(sale_split[[i]], !is.na(.data[[var]])))
        }
        sale_split
    }
    for (i in carry_vars) {
        sale_split <- carry_one_variable(sale_split, yrs, i)
    }
    sale_split
}

#' Internal Function: Carry multi-year/lifetime durations forward
#' 
#' This function is intended to be called from \code{\link{make_lic_history}}.
#' 
#' @param sale_split list: ranked sale table split by year
#' @inheritParams make_lic_history
#' @import dplyr
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' 
#' sale_unranked <- left_join(lic, sale)
#' sale_ranked <- rank_sale(sale_unranked) 
#' sale_split <- select(sale_ranked, cust_id, year, duration) %>%
#'     split(sale_ranked$year)
#'     
#' carry_duration(sale_split, 2008:2019)
carry_duration <- function(sale_split, yrs) {
    if (any(!c("cust_id", "year", "duration") %in% colnames(sale_split[[1]]))) {
        stop("All 3 variables (cust_id, year, duration) needed ",
             "for make_lic_history()", call. = FALSE)
    }
    for (i in 2L:length(yrs)) {
        # carry forward previous year
        sale_split[[i]] <- sale_split[[i-1]] %>%
            filter(.data$duration != 1) %>%
            mutate(lag_duration = .data$duration - 1) %>%
            select(.data$cust_id, .data$lag_duration) %>%
            
            # join to current year & pick highest duration
            full_join(sale_split[[i]], by = "cust_id") %>%
            mutate(
                duration = pmax(.data$duration, .data$lag_duration, na.rm = TRUE),
                year = yrs[i]
            ) %>%
            select(-.data$lag_duration)
    }
    sale_split
}

make_lic_history_old <- function(sale_ranked, yrs, carry_vars = NULL) {
    
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
#' Intended to be called following \code{\link{make_lic_history}}, creates the
#' categorical variable, R3 (1=carried, 2=renewed, 3=reactivated, 4=recruited), 
#' where "retained" consists of carried + renewed (hence R3: retain, reactivate, recruit).
#' 
#' @param lic_history license history data frame produced with \code{\link{make_lic_history}} 
#' @inheritParams make_lic_history
#' @param show_summary logical: if TRUE, print a tabular summary that shows R3 category
#' by the number of years since a license was held.
#' @param show_check_vars logical: if TRUE, include output variables used in summary
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' x <- history %>%
#'     select(-R3) %>%
#'     identify_R3(2008:2019, show_summary = TRUE)
#' 
#' # calculate % of license holders who are recruits
#' group_by(x, year, R3) %>% 
#'     summarise(n = n()) %>% 
#'     mutate(pct = n / sum(n)) %>%
#'     filter(R3 == 4, year != 2019)
identify_R3 <- function(lic_history, yrs, show_summary = FALSE, show_check_vars = FALSE) {
    # setup - last year held to calcuate "yrs_since" (for identifying recruits)
    lic_history <- lic_history %>%
        arrange(.data$cust_id, .data$year) %>%
        group_by(.data$cust_id) %>%
        mutate(lag_year = lag(.data$year), lag_duration_run = lag(.data$duration_run)) %>%
        ungroup() %>%
    
        # calculate
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
    # wrap up
    if (show_summary) {
        check_identify_R3(lic_history, yrs) %>% print()
    }
    if (show_check_vars) {
        select(lic_history, -.data$lag_year)
    } else {
        select(lic_history, -.data$lag_year, -.data$lag_duration_run, -.data$yrs_since)
    }
}

#' Identify lapse group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} 
#' and codes lapse (0 = renews next year, 1 = lapses next year).
#' 
#' @inheritParams identify_R3
#' @param show_summary logical: if TRUE, include a tabular summary that shows lapse 
#' identification based on how many years until the next license is held.
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' history_yrs <- 2008:2019
#' lapse_yrs <- 2008:2018 # must exclude incomplete 2019
#' 
#' x <- history %>%
#'     select(-lapse) %>%
#'     identify_lapse(lapse_yrs, show_summary = TRUE) 
#' 
#' # calculate annual churn rate
#' group_by(x, year) %>% 
#'     summarise(mean(lapse)) %>%
#'     # Note: this year's churn is based on lapse summary from the previous year
#'     # since the lapse variable identifies whether a customer lapses next year
#'     mutate(year = year + 1)
identify_lapse <- function(lic_history, yrs, show_summary = FALSE, show_check_vars = FALSE) {
    # setup - next year a license is held (to easily identify lapse)
    lic_history <- lic_history %>%
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
    # wrap up
    if (show_summary) {
        check_identify_lapse(lic_history) %>% print()
    }
    if (show_check_vars) {
        lic_history
    } else {
        select(lic_history, -.data$lead_year)
    }
}

# Checking & Summarizing --------------------------------------------------

#' Sample the output of \code{\link{make_lic_history}}
#'
#' View a sample of customers from license history table to check the 
#' year over year dynamics (outputs a list split by customer ID).
#' 
#' @param n_samp numeric: number of customers to view
#' @param buy_min numeric: minimum number of license purchases for customers to include
#' @param buy_max numeric: maximum number of license purchases for customers to include
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' data(history)
#' check_history_samp(history, n_samp = 5)
check_history_samp <- function(lic_history, n_samp = 3, buy_min = 3, buy_max = 8) {
    samp <- lic_history %>%
        count(.data$cust_id) %>%
        filter(.data$n >= buy_min, .data$n <= buy_max) %>%
        sample_n(n_samp) %>%
        left_join(lic_history, by = "cust_id") %>%
        select(-.data$n) %>%
        data.frame()
    split(samp, samp$cust_id)
}

#' Internal Function: Check R3 Identification
#'
#' Intended to be run as part of \code{\link{identify_R3}} (where show_summary = TRUE).
#' Produces a count summary of customers by R3, yrs_since, & lag_duration_run.
#' 
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' select(history, -R3) %>%
#'     identify_R3(2008:2019, show_check_vars = TRUE) %>%
#'     check_identify_R3(2008:2019)
check_identify_R3 <- function(lic_history, yrs) {
    if (!"yrs_since" %in% names(lic_history)) {
        warning("yrs_since variable needed for check_identify_R3 ",
                "(see ?identify_R3", call. = FALSE)
        return(invisible())
    }
    lic_history %>%
        filter(.data$year > yrs[5]) %>%
        mutate(R3_label = factor_R3(.data$R3)) %>%
        count(.data$R3, .data$R3_label, .data$yrs_since, .data$lag_duration_run) %>%
        data.frame()
}

#' Internal Function: Check lapse identification
#'
#' Intended to be run as part of \code{\link{identify_lapse}} (where show_summary = TRUE).
#' Produces a count summary of customers by lapse and lead_year.
#' 
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' select(history, -lapse) %>%
#'     filter(year %in% 2008:2018) %>%
#'     identify_lapse(2008:2018, show_check_vars = TRUE) %>%
#'     check_identify_lapse()
check_identify_lapse <- function(lic_history) {
    if (!"lead_year" %in% names(lic_history)) {
        warning("lead_year variable needed for check_identify_lapse ",
                "(see ?identify_lapse", call. = FALSE)
        return(invisible())
    }
    lapse_summary <- lic_history %>%
        mutate(yrs_till_next = case_when(
            is.na(.data$lapse) ~ "C. Next time held: unknown",
            .data$lead_year - .data$year == 1 ~ "A. Next time held: 1yr (i.e., renewed)",
            TRUE ~ "B. Next time held: >1yr/never (i.e., lapsed)"
        )) %>%
        count(.data$lapse, .data$year, .data$yrs_till_next) %>%
        data.frame()
    lapse_summary %>%
        select(-.data$yrs_till_next) %>%
        split(lapse_summary$yrs_till_next)
}
