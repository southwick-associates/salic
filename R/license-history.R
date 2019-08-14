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

#' Internal Function: Check years range & sort
#' 
#' Prints a warning any of the specified years aren't included in the dataset and 
#' returns a vector of availabe years, sorted ascending. The sort ensures correct 
#' ordering in subsequent license history calculations, which include iterations
#' by year that would produce incorrect results if not sorted.
#' 
#' This function is intended to be called from \code{\link{make_lic_history}},
#' \code{\link{identify_R3}}, or \code{\link{identify_lapse}}.
#' 
#' @param df data frame: table that contains "year" variable
#' @param func_name character: name of function to print in warning
#' @inheritParams make_lic_history
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' data(sale)
#' prep_yrs(c(2010, 2008, 2015), sale, "my_function")
#' prep_yrs(c(2007, 2015, 2010), sale, "my_function") # print a warning
prep_yrs <- function(yrs, df, func_name) {
    yrs <- sort(yrs)
    if (any(!yrs %in% unique(df$year))) {
        yrs_specified <- yrs 
        yrs <- dplyr::intersect(yrs, unique(df$year))
        warning(
            "Certain yrs in ", func_name, " are missing from the input table:\n", 
            "- Years specified: ", paste(yrs_specified, collapse = ", "), "\n",
            "- Years used:      ", paste(yrs, collapse = ", "), call. = FALSE
        )
    }
    yrs
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
#'     make_lic_history(2008:2019, carry_vars = c("month", "res"))
#' 
#' # check a sample of several customers
#' check_history_samp(history)
make_lic_history <- function(sale_ranked, yrs, carry_vars = NULL) {
    yrs <- prep_yrs(yrs, sale_ranked, "make_lic_history()")
    x <- sale_ranked[c("cust_id", "year", "duration", carry_vars)] %>%
        filter(.data$year %in% yrs)
    split(x, x$year) %>%
        carry_duration(yrs) %>%
        carry_variables(yrs, carry_vars) %>%
        bind_rows() %>% 
        rename(duration_run = duration)
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
                duration = pmax(.data$duration, .data$lag_duration, na.rm = TRUE) %>%
                    as.integer(),
                year = yrs[i]
            ) %>%
            select(-.data$lag_duration)
    }
    sale_split
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
        stop("All carry_vars (", paste(carry_vars, collapse = ", "),
             ") needed for make_lic_history()", call. = FALSE)
    }
    # replace missings (of var) with value from previous year (if available)
    for (var in carry_vars) {
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
    }
    sale_split
}

#' Identify R3 group each year
#'
#' Intended to be called following \code{\link{make_lic_history}}, creates the
#' categorical variable, R3 (1=carried, 2=renewed, 3=reactivated, 4=recruited), 
#' where "retained" consists of carried + renewed (hence R3: retain, reactivate, recruit).
#' 
#' @param lic_history data frame: output of \code{\link{make_lic_history}} 
#' @param yrs numeric: years used for identifying R3 (note that the first 5 years 
#' will all have missing values for R3 output)
#' @param show_summary logical: if TRUE, print a tabular summary that shows R3 category
#' by the number of years since a license was held.
#' @param show_check_vars logical: if TRUE, include output variables used in summary
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' x <- select(history, -R3) %>%
#'     identify_R3(show_summary = TRUE)
#' 
#' # calculate % of license holders who are recruits
#' group_by(x, year, R3) %>% 
#'     summarise(n = n()) %>% 
#'     mutate(pct = n / sum(n)) %>%
#'     filter(R3 == 4, year != 2019)
identify_R3 <- function(
    lic_history, yrs = sort(unique(lic_history$year)),
    show_summary = FALSE, show_check_vars = FALSE
) {
    yrs <- prep_yrs(yrs, lic_history, "identify_R3()")
    
    # get lag year for R3 identification
    dt <- data.table(lic_history)
    cols <- c("year", "duration_run")
    anscols <- paste("lag", cols, sep = "_")
    dt[order(year), (anscols) := shift(.SD, 1), by = cust_id, .SDcols = cols]
    
    lic_history <- as_tibble(dt) %>%
        mutate(
            yrs_since = .data$year - .data$lag_year,
            R3 = case_when(
                .data$year <= yrs[5] ~ NA_integer_, # 1st 5 yrs shouldn't be identified
                is.na(.data$yrs_since) | .data$yrs_since > 5 ~ 4L, # recruited
                .data$yrs_since == 1 & .data$lag_duration_run > 1 ~ 1L, # carried
                .data$yrs_since == 1 ~ 2L, # renewed
                TRUE ~ 3L # otherwise reactivated
            )
        ) 
    if (show_summary) {
        check_identify_R3(lic_history, yrs) %>% print()
    }
    if (!show_check_vars) {
        lic_history <- select(lic_history, -.data$lag_duration_run, -.data$yrs_since)
    }
    select(lic_history, -.data$lag_year)
}

#' Identify lapse group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} 
#' and codes lapse (0 = renews next year, 1 = lapses next year).
#' 
#' @inheritParams identify_R3
#' @param yrs numeric: years used for identifying lapse (note that the last year will
#' have all missing values for lapse output)
#' @param show_summary logical: if TRUE, include a tabular summary that shows lapse 
#' identification based on how many years until the next license is held.
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' # excluding partial year 2019 since churn is only estimated for full-year results
#' history <- filter(history, year != 2019)
#' 
#' x <- select(history, -lapse) %>%
#'     identify_lapse(show_summary = TRUE) 
#' 
#' # calculate annual churn rate
#' # (note that a given year's churn is based on lapse summary from the previous year)
#' group_by(x, year) %>% 
#'     summarise(mean(lapse)) %>%
#'     mutate(year = year + 1)
identify_lapse <- function(
    lic_history, yrs = sort(unique(lic_history$year)), 
    show_summary = FALSE, show_check_vars = FALSE
) {
    yrs <- prep_yrs(yrs, lic_history, "identify_lapse()")
    
    # issue warning if low final year
    cnt <- count(lic_history, .data$year) %>%
        filter(.data$year %in% yrs) %>%
        mutate(pct_change = (.data$n - lag(.data$n)) / lag(.data$n) * 100)
    last_change <- cnt$pct_change[length(yrs)] %>% round(1)
    if (last_change < -20) {
        warning(
            "There is a large drop in the final year specified: ", last_change, 
            "%\n- Please ensure all specified yrs are complete for lapse identification.", 
            call. = FALSE
        )
    }
    # get lead year for lapse identification
    dt <- data.table(lic_history)
    cols <- "year"
    anscols <- paste("lead", cols, sep = "_")
    dt[order(year), (anscols) := shift(.SD, 1, type = "lead"), by = cust_id, .SDcols = cols]
    
   lic_history <- as_tibble(dt) %>%
        mutate(lapse = case_when( 
            .data$year >= max(yrs) ~ NA_integer_, 
            .data$lead_year == (.data$year + 1) ~ 0L, # renewed 
            TRUE ~ 1L # lapsed 
        ))
    if (show_summary) {
        check_identify_lapse(lic_history) %>% print()
    }
    if (!show_check_vars) {
        lic_history <- select(lic_history, -.data$lead_year)
    }
    lic_history
}

identify_lapse_old <- function(
    lic_history, yrs = sort(unique(lic_history$year)), 
    show_summary = FALSE, show_check_vars = FALSE
) {
    yrs <- prep_yrs(yrs, lic_history, "identify_lapse()")
    
    # issue warning if low final year
    cnt <- count(lic_history, .data$year) %>%
        filter(.data$year %in% yrs) %>%
        mutate(pct_change = (.data$n - lag(.data$n)) / lag(.data$n) * 100)
    last_change <- cnt$pct_change[length(yrs)] %>% round(1)
    if (last_change < -20) {
        warning(
            "There is a large drop in the final year specified: ", last_change, 
            "%\n- Please ensure all specified yrs are complete for lapse identification.", 
            call. = FALSE
        )
    }
    lic_history <- lic_history %>%
        arrange(.data$cust_id, .data$year) %>% # for correct lead ordering
        group_by(.data$cust_id) %>% # to ensure lead calculations are customer-specific
        mutate(lead_year = lead(.data$year)) %>%
        ungroup() %>%
        mutate(lapse = case_when( 
            .data$year >= max(yrs) ~ NA_integer_, 
            .data$lead_year == (.data$year + 1) ~ 0L, # renewed 
            TRUE ~ 1L # lapsed 
        ))
    if (show_summary) {
        check_identify_lapse(lic_history) %>% print()
    }
    if (!show_check_vars) {
        lic_history <- select(lic_history, -.data$lead_year)
    }
    lic_history
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
