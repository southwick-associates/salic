# for summarizing license history to easily calculate R3, churn
# ultimate goal - making a privilege table with lapse & R3 variables


#' Aggregate a Sale Table to insure 1 row per customer per year.
#'
#' This function takes an input sale table and returns the max duration
#' value present in each year (per customer) - essentially a ranking by
#' license duration.
#' @param sale data frame: Input sales data
#' @param rank_var character: name of variable(s) to use for ranking
#' @import dplyr
#' @family functions for summarizing license history
#' @seealso \code{\link{check_rank_sale}}
#' @export
#' @examples
#' ### Run ranking with sample data
#' library(dplyr)
#' library(tidyr)
#'
#' # Join duration variable to sale data
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#'
#' # Perform ranking and check the result
#' sale_ranked <- rank_sale(sale_unranked)
#' check_rank_sale(sale_ranked, sale_unranked)
rank_sale <- function(sale, rank_var = "duration") {
    # This executes quickly
    select_(sale, .dots = c("cust_id", "year", rank_var)) %>%
        group_by(cust_id, year) %>%
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
#' Output table is stored in a list of data frames (one per license year). It is
#' intended  as intermediate output for making a privilege table using
#' \code{\link{make_priv}}.
#'
#' The input data frame (sale) must have at least 3 columns
#' (cust_id, year, duration) and should only have 1 record per customer per year
#' (which can be insured by running \code{\link{rank_sale}}).
#'
#' Each data frame in the output list has 3 columns:
#' \itemize{
#' \item \emph{cust_id}: Customer ID
#' \item \emph{duration}: Bought a license this year? (>=1: yes, ==0: no),
#' where duration = 1 refers to annual or short-term, 2 to 2 yr, 3 to 3 yr, ...,
#' duration = 99 is used to indicate lifetime licenses
#' \item \emph{left}: Time remaining on previous license  (==0: need to renew,
#' >0: don't need to renew, ==NA: haven't bought before, <0 didn't buy last year)
#' }
#' @param sale_ranked data frame: Sales table from which license history will be made
#' @param yrs numeric: Years in sales data (column 'year') from which
#' to create license history
#' @inheritParams update_track
#' @import dplyr
#' @family functions for summarizing license history
#' @export
#' @examples
#' # Make tracking table using sample data (and check)
#' # this makes a tracking table that matches the one pre-loaded in sample data
#' track <- make_track(sale_ranked, 2004:2013)
#' check_make_track(track, sale_ranked)
make_track <- function(sale_ranked, yrs, carry_vars = NULL) {
    # initialize tracking in first year of data
    # each tracking table (1 per year) has the same number of rows (1 per customer)
    track <- list()
    i <- yrs[1]
    cust <- select(sale_ranked, cust_id) %>% distinct()
    track[[1]] <- cust %>%
        mutate(left = as.numeric(NA)) %>%            # initialize counter
        left_join(filter(sale_ranked, year == yrs[1])) %>%  # get year 1 sales
        mutate(duration = ifelse(is.na(duration), 0, duration)) %>%
        select(-year)
    # update tracking
    for (i in 2:length(yrs)) {
        track[[i]] <- update_track(track[[i-1]], sale_ranked, yrs[i], carry_vars)
    }
    names(track) = as.character(yrs)
    track
}

#' Update license history data for a single year
#'
#' This is called as a part of \code{\link{make_track}}
#' @param track data frame: tracking table with a variable ('left') which is
#' updated based on purchases in the current year
#' @param sale_ranked data frame: sale table showing purchases for all years
#' @param yr numeric: current year, used to subset sale data for updating tracking
#' @param carry_vars character: variables to carry over from previous year
#' (for multi-year and lifetime licenses)
#' @import dplyr
#' @family functions for summarizing license history
#' @export
#' @examples
#' update_track()
update_track <- function(x, sale_ranked, yr, carry_vars = NULL) {
    out <- x %>%
        # move the "left" counter down by 1 year
        # those who did buy last year (!is.na(duration)) are set based on
        # the duration of last year's licenes
        mutate(
            left = ifelse(duration == 0, left - 1,    # didn't buy last year
                          duration - 1)) %>%          # did buy last year
        select(cust_id, left) %>%
        # update duration with current year's data
        left_join(filter(sale_ranked, year == yr)) %>%
        mutate(duration = ifelse(is.na(duration), 0, duration)) %>%
        select(-year)
    # carry over previous value(s) for multi-year/lifetime licenses
    # for example, residency
    if (!is.null(carry_vars)) {
        replace_current <- function(current, last, var) {
            last <- select_(last, .dots <- c("cust_id"), var)
            last[["oldvar"]] <- last[[var]]
            last[[var]] <- NULL
            current <- left_join(current, last, by = "cust_id")
            current[[var]] <- ifelse(current$left > 0 & !is.na(current$left),
                                     current$oldvar, current[[var]])
            select(current, -oldvar)
        }
        for (i in carry_vars) { out <- replace_current(out, x, i) }
    }
    out
}

#' Make a Privilege Table from License History
#'
#' Tags churn/renewal and R3 categories at the customer scale over a set of years.
#' This allows efficient summarization of these metrics at any aggregate scale.
#'
#' Each data frame in the output list has 3 columns:
#' \itemize{
#' \item \emph{cust_id}: Customer ID
#' \item \emph{lapse}: Lapsed next year? (==1: yes, ==0: no)
#' \item \emph{R3}: R3 group this year (==0: carry, ==1: retain, ==2: reactivate, ==3: recruit)
#' }
#' @param track list: Set of license history tables created using
#' \code{\link{make_track}}
#' @family functions for summarizing license history
#' @export
#' @examples
#' make_priv()
make_priv <- function(track) {
    x <- list()  # one table per year (probably stack at the end)
    for (i in seq_along(track)) {
        # identify churn (lapse vs. renew in next year)
        if (i != length(track)) {
            x[[i]] <- get_lapse(track, i)
        } else {
            # can't calculate churn for final year
            x[[i]] <- mutate(track[[i]], lapse = as.numeric(NA))
        }
        # identify R3
        if (i > 5) {
            x[[i]] <- get_R3(x, i)
        } else {
            # can't identify R3 == "recruited" without 5 yrs of data
            x[[i]]$R3 <- as.numeric(NA)
        }

    }
    names(x) <- names(track)
    x
}

#' Identify R3 group each year
#'
#' This is called as part of \code{\link{make_priv}} and codes R3: 0 = carried,
#' 1 = retained, 2 = reactivated, 3 = recruited, where 1 (retained) consists
#' of carried + renewed.
#'
#' R3 Conditionals:
#' \itemize{
#' \item \emph{0 (carry)}: License doesn't expire this year
#' (l!is.na(left) & left > 0)
#' \item \emph{1 (renew)}: Renewed a license this year
#' (duration >=1 & !is.na(left) & left == 0)
#' \item \emph{2 (reactivate)}: Bought a license, no privilege last year but had
#' a privilege within the past 5 years
#' (duration >=1 & !is.na(left) & left < 0 & left > -5)
#' \item \emph{3 (recruit)}: Bought a license, but no privilege in past 5 years
#' (duration >=1 & (is.na(left) | left <= -5))
#' }
#' @param i integer/character: position of data frame in tracking list to
#' calculate metric
#' @inheritParams make_priv
#' @import dplyr
#' @family functions for summarizing license history
#' @export
#' @examples
#' # Calculate R3 in 2012
#' library(dplyr)
#' x <- get_R3(track, "2012")
#'
#' # Show counts of customers who bought licenses in 2012
#' # Recruited (R3=3) bought never (NA) or 6 or more years previously
#' # Reactivated (R3=2) bought between 2 and 5 years before
#' # Renewed (R3=1) bought last year
#' # Carried (R3=0) bought a multi-year or lifetime license
#' count(x, duration, left, R3) %>%
#'     filter(!is.na(R3)) %>%
#'     tidyr::spread(duration, n, fill = "")
get_R3 <- function(track, i) {
    # the !is.na(left) condition is specified to prevent output being
    # assigned to missing values unintentionally
    mutate(track[[i]],
           R3 = ifelse(!is.na(left) & left > 0, 0, # carry
                ifelse(duration >=1 & !is.na(left) & left == 0, 1, # renew
                ifelse(duration >=1 & !is.na(left) & left < 0 & left > -5, 2, # reactivate
                ifelse(duration >=1 & (is.na(left) | left <= -5), 3, # recruit
                NA)))))
}

#' Identify lapse group each year
#'
#' This is called as part of \code{\link{make_priv}} and codes lapse:
#' 0 = renew next year, 1 = lapse next year.
#'
#' Lapse Conditionals:
#' \itemize{
#' \item \emph{1 (lapse)}: Didn't buy a license next year
#' (duration == 0 & left == 0)
#' \item \emph{0 (renew)}: Did buy a license next year
#' (duration >= 1 & left == 0)
#' \item \emph{0 (carry)}: Multi-year or lifetime license next year
#' (left > 0 & !is.na(left))
#' }
#' @inheritParams get_R3
#' @param count_multi logical: If TRUE, carried group (multi-year, lifetime) is
#' counted as renewed for calculating lapse (churn)
#' @import dplyr
#' @family functions for summarizing license history
#' @export
#' @examples
#' # Calculate lapse in 2012
#' library(dplyr)
#' x <- get_lapse(track, "2012")
#' count(x, duration, left, lapse) %>%
#'     filter(!is.na(R3)) %>%
#'     tidyr::spread(duration, n, fill = "") %>% data.frame()
get_lapse <- function(track, i, count_multi = TRUE) {
    # the !is.na(left) condition is specified to prevent output being
    # assigned to missing values unintentionally
    if (count_multi) {
        # carried group is counted as renewed
        x <- track[[i+1]] %>% mutate(
            lapse = ifelse(duration == 0 & left == 0 & !is.na(left), 1, # lapsed
                    ifelse(duration >= 1 & left == 0 & !is.na(left), 0, # renewed
                    ifelse(left > 0 & !is.na(left), 0, # carried
                    NA))))
    } else {
        x <- track[[i+1]] %>% mutate(
            lapse = ifelse(duration == 0 & left == 0 & !is.na(left), 1, # lapsed
                    ifelse(duration >= 1 & left == 0 & !is.na(left), 0, # renewed
                    NA)))
    }
    x <- select(x, cust_id, lapse)
    left_join(track[[i]], x)
}

#' Finalizes input privilege table (stored as list)
#'
#' This returns a final privilege table - stored as a data frame. Drops rows
#' that don't represent license privileges & finalizes variables
#'
#' The output data frame has 4 columns:
#' \itemize{
#' \item \emph{cust_id}: Customer ID
#' \item \emph{year}: License Year
#' \item \emph{lapse}: Lapsed next year? (==1: yes, ==0: no)
#' \item \emph{R3}: R3 group this year (Carried, Renewed, Reactivated, Recruited)
#' }
#' @param priv list: Input privilege table
#' @param keep character: Variables to keep in the cleaned table
#' @import dplyr
#' @family functions for summarizing license history
#' @export
#' @examples
#' make_priv_final()
make_priv_final <- function(priv, keep = c("cust_id", "year", "lapse", "R3")) {
    for (i in seq_along(priv)) {
        yr <- names(priv)[i]
        # drop rows that don't correspond to privileges in a given year
        priv[[i]] <- priv[[i]] %>%
            filter(duration >= 1 | left > 0) %>%
            mutate(year = yr) %>%
            select_(.dots = keep)
    }
    out <- bind_rows(priv)
    # out$R3 <- factor(out$R3,
    #                  labels = c("Carried", "Renewed", "Reactivated", "Recruited"))
    out
}
