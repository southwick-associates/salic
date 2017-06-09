# Deprecated functions

# for summarizing license history to easily calculate R3, churn
# ultimate goal - making a privilege table with lapse & R3 variables

# Making Tables -----------------------------------------------------------

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
#' @family deprecated license history functions
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
#' @family deprecated license history functions
#' @export
#' @examples
#' update_track()
update_track <- function(x, sale_ranked, yr, carry_vars = NULL) {
    out <- x %>%
        # move the "left" counter down by 1 year
        # done based on the value of duration from the previous year
        #  duration == 0 (didn't buy last year) > move down (left - 1), NAs remain NAs
        #  otherwise (did buy last year) > move down (duration - 1)
        
        ### DECIDE LATER WHETHER TO MAKE THIS ADJUSTMENT
        # this was written to fix a bug where left gets reset if a person buys a new privilege
        # it hasn't been tested yet, so I didn't make it operational
        # exception: some multi-year/lifetime license holders will still buy other licenses
        #  left > 0
        # the left counter will always try to take the maximum reasonable value
        # example: if a person buys a fishing priv but bought a 3-year license the previous year
        #   the left value will be updated to 1 (instead of 0)
        # mutate(left = ifelse(
        #     duration == 0 | (!is.na(left) & left > 0), left - 1,   # didn't buy last year
        #     max(duration - 1, left - 1, na.rm = T)                 # did buy last year
        # )) %>%
        
        mutate(left = ifelse(
            duration == 0, left - 1,   # didn't buy last year
            duration - 1               # did buy last year
        )) %>%
        select(cust_id, left) %>%
        
        # update duration with current year's data
        left_join(filter(sale_ranked, year == yr)) %>%
        
        # set duration to zero for anyone who didn't buy a license
        # this makes it easier to specify the code to update the left variable
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
#' @family deprecated license history functions
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
#' This is called as part of \code{\link{make_priv}} and codes R3: 1 = carried,
#' 2 = retained, 3 = reactivated, 4 = recruited, where 1 (retained) consists
#' of carried + renewed.
#'
#' R3 Conditionals:
#' \itemize{
#' \item \emph{1 (carry)}: License doesn't expire this year
#' (l!is.na(left) & left > 0)
#' \item \emph{2 (renew)}: Renewed a license this year
#' (duration >=1 & !is.na(left) & left == 0)
#' \item \emph{3 (reactivate)}: Bought a license, no privilege last year but had
#' a privilege within the past 5 years
#' (duration >=1 & !is.na(left) & left < 0 & left > -5)
#' \item \emph{4 (recruit)}: Bought a license, but no privilege in past 5 years
#' (duration >=1 & (is.na(left) | left <= -5))
#' }
#' @param i integer/character: position of data frame in tracking list to
#' calculate metric
#' @inheritParams make_priv
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' # Calculate R3 in 2012
#' library(dplyr)
#' x <- get_R3(track, "2012")
#'
#' # Show counts of customers who bought licenses in 2012
#' # Recruited (R3=4) bought never (NA) or 6 or more years previously
#' # Reactivated (R3=3) bought between 2 and 5 years before
#' # Renewed (R3=2) bought last year
#' # Carried (R3=1) bought a multi-year or lifetime license
#' count(x, duration, left, R3) %>%
#'     filter(!is.na(R3)) %>%
#'     tidyr::spread(duration, n, fill = "")
get_R3 <- function(track, i) {
    # the !is.na(left) condition is specified to prevent output being
    # assigned to missing values unintentionally
    mutate(track[[i]],
           R3 = ifelse(!is.na(left) & left > 0, 1, # carry
                ifelse(duration >=1 & !is.na(left) & left == 0, 2, # renew
                ifelse(duration >=1 & !is.na(left) & left < 0 & left > -5, 3, # reactivate
                ifelse(duration >=1 & (is.na(left) | left <= -5), 4, # recruit
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
#' @family deprecated license history functions
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
#' @family deprecated license history functions
#' @export
#' @examples
#' make_priv_final()
make_priv_final <- function(
    priv, 
    keep = c("cust_id", "year", "lapse", "R3", "res", "sex", "age", "county")
) {
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




# Checking & Summarizing --------------------------------------------------

#' Check license history (stored in a list)
#' @param x list: license history tables returned by \code{\link{make_track}}
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' check_track()
check_track <- function(x) {
    y <- list()
    for (i in seq_along(x)) {
        yr <- names(x)[i]
        y[[i]] <- count(x[[i]], duration, left) %>% mutate(year = yr)
    }
    bind_rows(y) %>% tidyr::spread(year, n, fill = "") %>% data.frame()
}

#' Check license history creation
#' @param track list: license history tables returned by \code{\link{make_track}}
#' @param sale data frame: sales table used to make license history
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' check_make_track(track, sale_ranked)
check_make_track <- function(track, sale) {
    catf("---CHECK LICENSE HISTORY SUMMARIZATION---")
    catf("Count of License Holders (Customers):")
    count <- list()
    yr <- list()
    for (i in seq_along(track)) {
        count[[i]] <- filter(track[[i]], duration != 0) %>% nrow()
        yr[[i]] <- names(track)[i]
    }
    data.frame(year = unlist(yr), n = unlist(count)) %>% printf()
    catf("License History by duration-left:")
    check_track(track) %>% printf()
}

#' Check privilege table (lapse)
#' @param x list: license privilege tables returned by \code{\link{make_priv}}
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' check_lapse()
check_lapse <- function(x) {
    y <- list()
    for (i in seq_along(x)) {
        if (i != length(x)) {
            yr <- names(x)[i]
            y[[i]] <- count(x[[i]], duration, left, lapse) %>% mutate(year = yr)
        }
    }
    bind_rows(y) %>% tidyr::spread(year, n, fill = "") %>% data.frame()
}

#' Check privilege table (R3)
#' @param x list: license privilege tables returned by \code{\link{make_priv}}
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' check_R3()
check_R3 <- function(x) {
    y <- list()
    for (i in seq_along(x)) {
        if (i > 5) {
            yr <- names(x)[i]
            y[[i]] <- count(x[[i]], duration, left, R3) %>% mutate(year = yr)
        }
    }
    bind_rows(y) %>% tidyr::spread(year, n, fill = "") %>% data.frame()
}

#' Check Privilege Table Creation
#' @param priv list: license privilege tables returned by \code{\link{make_priv}}
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' check_make_priv()
check_make_priv <- function(priv) {
    catf("---CHECK PRIVILEGE SUMMARIZATION---")
    catf("R3 identification:")
    check_R3(priv) %>% printf()
    catf("Lapse identification:")
    check_lapse(priv) %>% printf()
}
