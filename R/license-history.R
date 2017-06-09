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
rank_sale <- function(sale, rank_var = "duration",  
                      grp_var = c("cust_id", "year")) {
    # This executes quickly
    group_by_(sale, .dots = grp_var) %>%
        # to insure highest value is picked - sort ascending, pick last
        arrange_(.dots = rank_var) %>%
        summarise_each(funs(last)) %>%
        ungroup()
    # Below is a more natural way to select highest duration
    # However it runs very slowly for some reason
    #select(sale, cust_id, year, duration) %>%
    #group_by(cust_id, year) %>%
    #summarise(duration = max(duration)) %>%
    #ungroup()
}

#' Make a License History Table
#'
#' Output table is stored in a data frame (n = C * Y). Where C = Number of 
#' Customers & Y = Number of Years
#'
#' The input data frame (sale) must have at least 3 columns
#' (cust_id, year, duration) and should only have 1 record per customer per year
#' (which can be insured by running \code{\link{rank_sale}}).
#'
#' Each data frame in the output data frame has 7 columns:
#' \itemize{
#' \item \emph{cust_id}: Customer ID
#' \item \emph{year}: Customer ID
#' \item \emph{duration}: Bought a license this year? (>=1: yes, ==0: no),
#' where duration = 1 refers to annual or short-term, 2 to 2 yr, 3 to 3 yr, ...,
#' duration = 99 is used to indicate lifetime licenses
#' \item \emph{duration_run}: Running duration, which is necessary for multi-year
#' and lifetime licenses
#' \item \emph{lag_duration_run}: Previous year duration_run. A temporary value
#' used in coding and for downstream checking
#' \item \emph{bought}: Bought a license this year? (TRUE, FALSE)
#' \item \emph{has_priv}: Has a privilege this year? (TRUE, FALSE)
#' }
#' @param sale_ranked data frame: Sales table from which license history will be made
#' @param yrs numeric: Years in sales data (column 'year') from which
#' to create license history
#' @param carry_vars character: variables to carry over from previous year
#' (for multi-year and lifetime licenses)
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
#' filter(lic_history, has_priv) %>%
#'     ggplot(aes(factor(year))) + 
#'     geom_bar(aes(fill = factor(duration_run)))
#' check_history_summary(lic_history)
#' check_history_samp(lic_history)
make_lic_history <- function(sale_ranked, yrs, carry_vars = NULL) {
    
    # 1. fill-in NAs for years where no license was purchased by customer in given year
    # this is a bit of a hacky way to fill all years for every customer (1 row per customer-year)
    # - which is used later to create a running duration
    x <- sale_ranked %>%
        select(cust_id, year, duration) %>%
        tidyr::spread(year, duration) %>%            # fills NAs for missing years
        tidyr::gather(year, duration, -cust_id) %>%  # brings back into tidy form
        mutate(year = as.integer(year)) %>%
        arrange(cust_id, year)
    
    # 2. initialize a list to store tracking table
    # looping over a list is (seemingly) simpler than iterating over data frame rows
    lic_history <- list()
    
    # 3. Initialize running duration for first year
    # no previous years to update from
    lic_history[[1]] <- x %>%
        filter(year == yrs[1]) %>%
        mutate(
            duration_run = duration, # years remaining on privilege
            bought = ifelse(!is.na(duration), TRUE, FALSE) # bought privilege this year?
        )
    
    # 4. Calculate running duration for subsequent years
    for (i in 2:length(yrs)) {
        lic_history[[i]] <- x %>%
            # a. join current year (i) to previous year (i-1) to get lag_duration_run
            filter(year == yrs[i]) %>%
            left_join(select(lic_history[[i-1]], cust_id, lag_duration_run = duration_run),  
                      by = "cust_id"
            ) %>%
            mutate(
                # b. make variable (bought): did customer buy a license in current year?
                bought = ifelse(!is.na(duration), TRUE, FALSE),
                
                # c. make variable (duration_run), coding depends on "bought" condition
                duration_run = ifelse(
                    # if (bought in current year) we use current year's value
                    # UNLESS it's smaller than the running duration (lag_duration_run - 1)
                    # - this avoids the situation where (for example) a 1-year priv would replace
                    # - a multi-year (or lifetime) license
                    # - (i.e., it always favors the license with the longest remaining duration)
                    bought, pmax(duration, lag_duration_run - 1, na.rm = TRUE),
                    
                    # otherwise we use the running duration
                    lag_duration_run - 1
                )
            )
    }
    
    # 5. combine into data frame & finalize
    out <- bind_rows(lic_history) %>%
        mutate(
            # make a dummy variable that indicates whether a customer has 
            #  a privilege in a given year
            has_priv = ifelse(duration_run >= 1 & !is.na(duration_run), TRUE, FALSE)
        ) %>%
        # select(-lag_duration_run) %>%
        arrange(cust_id, year)
    
    # 6. carry over variables that need to be populated for multi-year & lifetime (e.g., res)
    # this is pretty clunky, would be nice if it could be simplified
    # the standard evaluation works, but it's a bit difficult to grok
    if (!is.null(carry_vars)) {
        for (var in carry_vars) {
            # initialize
            y <- sale_ranked %>%
                select_(.dots = c("cust_id", "year", var))
            
            # get the latest non-missing value of var
            # using standard evaluation to pass as.name(var)
            var_expression <- lazyeval::interp(
                ~ last(x),
                x = as.name(var)
            )
            first_var <- y %>%
                filter_(paste0("!is.na(", var, ")")) %>%
                group_by(cust_id) %>%
                # summarise_("first_var" = paste0("last(", var, ")")) # alternative
                summarise_(.dots = setNames(list(var_expression), "first_var"))
            
            # update missing values where appropriate
            var_expression <- lazyeval::interp(
                ~ ifelse(has_priv & !bought, first_var, x), 
                x = as.name(var)
            )
            out <- out %>%
                left_join(y, by = c("cust_id", "year")) %>%
                left_join(first_var, by = "cust_id") %>%
                mutate_(.dots = setNames(list(var_expression), var)) %>%
                select(-first_var)
        }
    }
    out
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
#' filter(lic_history, has_priv) %>%
#'     ggplot(aes(factor(year))) + 
#'     geom_bar(aes(fill = factor(R3)))
#' check_identify_R3(lic_history)
#' check_history_samp(lic_history)
identify_R3 <- function(lic_history, yrs) {
    lic_history %>%
        arrange(cust_id, year) %>% # for correct lag ordering
        group_by(cust_id) %>% # to insure any lagged calculations are customer-specific
        mutate(
            # make recruit designators to simplify R3 logic below
            # "new" haven't bought before, and "old" haven't bought in 5 years
            new_recruit = ifelse(is.na(lag(duration_run) & bought), TRUE, FALSE),
            old_recruit = ifelse(bought & lag_duration_run <= -4 & !is.na(lag_duration_run),
                                 TRUE, FALSE),
            
            # CARRY (1): still have time left on their previous purchase
            R3 = ifelse(lag_duration_run > 1 & !is.na(lag_duration_run), 1,
                        
            # RENEW (2): bought in current and had priv in previous year
            ifelse(bought & lag(has_priv), 2,
                   
            # RECRUIT (4): coded before reactivate so that logic is simpler
            ifelse(new_recruit | old_recruit, 4, 
                          
            # REACTIVATE (3): bought this year bought not in previous
            ifelse(bought & !(lag(bought)), 3, # reactivate
             
            # doesn't have a priv this year (has_priv == FALSE)
            NA)))),

            # set to NA for first 5 years
            R3 = ifelse(!(year %in% yrs[1:5]), R3, NA)
        ) %>%
        ungroup() %>%
        select(-new_recruit, -old_recruit)
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
#' filter(lic_history, has_priv) %>%
#'     ggplot(aes(factor(year))) + 
#'     geom_bar(aes(fill = factor(lapse)))
#' check_identify_lapse(lic_history)
#' check_history_samp(lic_history)
identify_lapse <- function(lic_history, yrs) {
    lic_history %>%
        arrange(cust_id, year) %>% # for correct lag ordering
        group_by(cust_id) %>% # to insure any lagged calculations are customer-specific
        mutate(
            # if didn't buy next year > lapsed, otherwise > renewed
            lapse = ifelse(has_priv & !(lead(has_priv)), 1, 0),
            
            # set to NA if no license bought in current year
            lapse = ifelse(has_priv, lapse, NA)
        ) %>%
        ungroup()
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
#' Check the output of \code{\link{make_lic_history}}
#'
#' A test to insure that the license history table was built correctly
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See ?salic::make_lic_history
check_history_summary <- function(lic_history) {
    lic_history %>%
        filter(has_priv) %>%
        count(year, bought, duration_run) %>%
        tidyr::spread(year, n) %>%
        arrange(desc(bought)) %>%
        data.frame()
}

#' Sample the output of \code{\link{make_lic_history}}
#'
#' Another test to insure that the license history table was built correctly
#' @inheritParams identify_R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' # See ?salic::make_lic_history
check_history_samp <- function(lic_history, n_samp = 3, buy_min = 3, buy_max = 8) {
    lic_history %>%
        filter(has_priv) %>%
        count(cust_id) %>%
        filter(n >= 3, n <= 8) %>%
        sample_n(n_samp) %>%
        left_join(lic_history, by = "cust_id") %>%
        select(-n, -lag_duration_run) %>%
        data.frame()
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
check_identify_R3 <- function(lic_history) {
    lic_history %>%
        filter(!(year %in% yrs[1:5])) %>%
        mutate(
            R3 = dplyr::recode(R3, `1` = "1-Carry", `2` = "2-Renew", 
                               `3` = "3-Reactivate", `4` = "4-Recruit")
        ) %>%
        count(has_priv, R3, lag_duration_run) %>% 
        tidyr::spread(R3, n) %>%
        data.frame()
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
        arrange(cust_id, year) %>%
        group_by(cust_id) %>%
        mutate(lead_has_priv = lead(has_priv)) %>%
        ungroup() %>%
        filter(has_priv) %>%
        count(lapse, year, has_priv, lead_has_priv) %>%
        tidyr::spread(year, n) %>%
        data.frame()
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
