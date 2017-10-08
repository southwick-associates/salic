# functions for making & checking license history
# including R3 and lapse coding

# Making Tables -----------------------------------------------------------

#' Aggregate a Sale Table to insure 1 row per customer per year.
#'
#' This function takes an input sale table and returns the max duration
#' value present in each year (per customer) - essentially a ranking by
#' license duration.
#' @param sale data frame: Input sales data
#' @param rank_var character: name of variable(s) to use for ranking
#' @param grp_var character: name of variable(s) used for grouping
#' @import dplyr
#' @family license history functions
#' @seealso \code{\link{check_rank_sale}}
#' @export
#' @examples
#' ### Run ranking with sample data
#' library(tidyverse)
#' data(lic, sale, package = "salic")
#'
#' # Join duration variable to sale data
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#'
#' # Perform ranking and check the result
#' sale_ranked <- rank_sale(sale_unranked)
#' check_rank_sale(sale_ranked, sale_unranked)
rank_sale <- function(sale, rank_var = "duration", grp_var = c("cust_id", "year")) {
    sale %>%
        # to insure highest value is picked - sort ascending, pick last
        arrange_(.dots = c(grp_var, rank_var)) %>%
        group_by_(.dots = grp_var) %>%
        slice(n()) %>%
        ungroup()
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
#' ### Run ranking with sample data
#' library(tidyverse)
#' library(salic)
#' data(lic, sale, package = "salic")
#'
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#' sale_ranked <- rank_sale(sale_unranked)
#' 
#' ### Make license history
#' yrs <- 2004:2013
#' lic_history <- make_lic_history(sale_ranked, yrs)
#' 
#' # check
#' ggplot_lic_history(lic_history, yrs, "duration_run")
#' check_history_samp(lic_history)
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
#' codes R3: 1 = carried, 2 = retained, 3 = reactivated, 4 = recruited, 
#' where 1 (retained) consists of carried + renewed.
#' @param lic_history license history data frame produced with \code{\link{make_lic_history}} 
#' @inheritParams make_lic_history
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' ### Run ranking with sample data
#' library(tidyverse)
#' library(salic)
#' data(lic, sale, package = "salic")
#'
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#' sale_ranked <- rank_sale(sale_unranked)
#' 
#' ### Make license history
#' yrs <- 2004:2013
#' lic_history <- make_lic_history(sale_ranked, yrs)
#' 
#' ### Identify R3
#' lic_history <- identify_R3(lic_history, yrs)
#' 
#' # check
#' ggplot_lic_history(lic_history, yrs, "R3")
#' check_identify_R3(lic_history, yrs)
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
#' @inheritParams get_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' ### Run ranking with sample data
#' library(tidyverse)
#' library(salic)
#' data(lic, sale, package = "salic")
#'
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#' sale_ranked <- rank_sale(sale_unranked)
#' 
#' ### Make license history
#' yrs <- 2004:2013
#' lic_history <- make_lic_history(sale_ranked, yrs)
#' 
#' ### Identify lapse
#' lic_history <- identify_lapse(lic_history, yrs)
#' 
#' # check
#' ggplot_lic_history(lic_history, yrs, "lapse")
#' check_identify_lapse(lic_history)
identify_lapse <- function(lic_history, yrs) {
    lic_history %>%
        arrange(cust_id, year) %>% # for correct lead ordering
        group_by(cust_id) %>% # to insure lead calculations are customer-specific
        mutate(
            lead_year= lead(year)
        ) %>%
        ungroup() %>%
        mutate(
            # if didn't buy next year: lapsed, otherwise: renewed
            lapse = ifelse(lead_year == (year + 1) & !is.na(lead_year), 0L, 1L),
            
            # set to NA for final year
            lapse = ifelse(year == yrs[length(yrs)], NA, lapse)
        )
}


# Checking & Summarizing --------------------------------------------------

#' Check the output of \code{\link{rank_sale}}
#'
#' A test to insure that sale ranking was performed correctly
#' @param sale_ranked data frame: Data produced after running
#' \code{\link{rank_sale}}
#' @param sale_unranked data frame: Data used as input to \code{\link{rank_sale}}
#' @param check_duration logical: If TRUE, runs a check to see if any
#' customers have mutliple license sales (with different durations) in the same
#' year (and insures these are ranked correctly if any are found). If this isn't
#' a concern, set to FALSE to avoid this relatively time-consuming test.
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See ?salic::rank_sale
check_rank_sale <- function(sale_ranked, sale_unranked, check_duration = TRUE) {
    # Was Ranking performed?
    catf("---CHECK SALES RANKING---")
    catf("Do any Customers have multiple sales in single year?:")
    catf2("(i.e., Was ranking performed in this operation?)")
    mult_test <- nrow(sale_unranked) != nrow(sale_ranked)
    print(mult_test)
    data.frame(category = c("Unranked Sales", "Ranked Sales"),
               count = c(nrow(sale_unranked), nrow(sale_ranked))) %>% printf()
    
    # Overall Comparison
    if (mult_test) {
        catf("Summary - Unranked Sales by year-duration:")
        catf2("(i.e., Total Sales Count)")
        count(sale_unranked, year, duration) %>%
            tidyr::spread(duration, n) %>% printf()
        catf("Summary - Ranked Sales by year-duration:")
        catf2("(i.e., Total Customer Count)")
        count(sale_ranked, year, duration) %>%
            tidyr::spread(duration, n) %>% printf()
    }
    
    # Were multiple duration values involved?
    if (mult_test & check_duration) {
        catf("Do any customers have multiple duration values in the same year?:")
        ### this filter operation is slow (not sure how to speed it up)
        mult <- count(sale_unranked, cust_id, year) %>%
            filter(n > 1) %>% select(-n)
        cust_diff <- left_join(mult, sale_unranked, by = c("cust_id", "year")) %>%
            group_by(cust_id, year) %>%
            filter(lag(duration) != duration) %>% ungroup()
        diff_test <- nrow(cust_diff) > 0
        print(diff_test)
        if (diff_test) {
            catf("Customers with multiple duration values (top 20):")
            select(head(cust_diff, 20), cust_id, year) %>%
                left_join(sale_unranked, by = c("cust_id", "year")) %>% printf()
            catf("Ranking of customers with multiple duration values (top 20):")
            catf2("(only the max duration value should be present)")
            select(head(cust_diff, 20), cust_id, year) %>%
                left_join(sale_ranked, by = c("cust_id", "year")) %>% printf()
        }
    }
}

#' Check the output of \code{\link{make_lic_history}} using a ggplot geom_bar
#'
#' A visual check of license history for a given variable (duration_run, R3, lapse)
#' @param var character: Name of variable to use for "fill" parameter in 
#'  \code{\link[ggplot2]{aes}} in \code{\link[ggplot2]{geom_bar}}
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See ?salic::make_lic_history
ggplot_lic_history <- function(lic_history, yrs, var = "duration_run") {
    lic_history %>%
        mutate_at(var, "factor") %>%
        ggplot2::ggplot(aes(factor(year))) + 
        ggplot2::geom_bar(ggplot2::aes_string(fill = var))
}

#' Sample the output of \code{\link{make_lic_history}}
#'
#' Look at a sample of customers from license history table to check the 
#' year over year dynamics
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See ?salic::make_lic_history
check_history_samp <- function(lic_history, n_samp = 3, buy_min = 3, buy_max = 8) {
    lic_history %>%
        count(cust_id) %>%
        filter(n >= 3, n <= 8) %>%
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
#' # See ?salic::identify_R3
check_identify_R3 <- function(lic_history, yrs) {
    # this function only produces an output if R3 has been identified
    # (which it might not have been if there aren't at least 5 years of data)
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
#' # See ?salic::identify_lapse
check_identify_lapse <- function(lic_history) {
    lic_history %>%
        mutate(
            yrs_till_next = ifelse(is.na(lead_year), "Never",  
                                   ifelse(lead_year - year == 1, "1", ">1"))
        ) %>%
        count(lapse, year, yrs_till_next) %>%
        tidyr::spread(year, n)
}


#' Check Final Privilege Table Creation
#' 
#' Produces a summary of the priv table with (1) total counts, (2) lapse, and (3) R3
#' @param priv data frame: license privilege table
#' @param rnd numeric: number of decimals to round pct change results
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' ### Run ranking with sample data
#' library(tidyverse)
#' library(salic)
#' data(lic, sale, package = "salic")
#'
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#' sale_ranked <- rank_sale(sale_unranked)
#' 
#' ### Make license history
#' yrs <- 2004:2013
#' lic_history <- make_lic_history(sale_ranked, yrs)
#' 
#' ### Identify R3 & lapse
#' lic_history <- identify_R3(lic_history, yrs)
#' lic_history <- identify_lapse(lic_history, yrs)
#' 
#' # make priv table & check
#' priv <- lic_history %>%
#'     filter(has_priv) %>%
#'     select(cust_id, year, R3, lapse)
#' check_make_priv_final(priv)
check_make_priv_final <- function(priv, rnd = 1) {
    if (is.grouped_df(priv)) {
        grp <- groups(priv)
    } else {
        grp <- NULL
    }
    grouping <- c(grp, "year")
    catf("---CHECK FINAL PRIVILEGE TABLE---")
    catf("Lapse Count:")
    count_(priv, c(grp, "year", "lapse")) %>%
        tidyr::spread(year, n, fill = "") %>%
        data.frame() %>% printf()
    catf("Lapse %:")
    count_(priv, c(grp, "year", "lapse")) %>%
        mutate(pct = round(n / sum(n) * 100, rnd)) %>%
        select(-n) %>% tidyr::spread(year, pct, fill = "") %>%
        data.frame() %>% printf()
    catf("R3 Count:")
    count_(priv, c(grp, "year", "R3")) %>%
        tidyr::spread(year, n, fill = "") %>%
        data.frame() %>% printf()
    catf("R3 %:")
    count_(priv, c(grp, "year", "R3")) %>%
        mutate(pct = round(n / sum(n) * 100, rnd)) %>%
        select(-n) %>% tidyr::spread(year, pct, fill = "") %>%
        data.frame() %>% printf()
    catf("Final Count of License Holders:")
    catf2("(will be larger than previous counts if there are multi-year or lifetime licenses)")
    count(priv, year) %>% data.frame() %>% printf()
}
