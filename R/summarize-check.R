# for checking license history summarization

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
#' @family functions for for checking license history summarization
#' @export
#' @examples
#' ### Test ranking on sample data
#' library(dplyr)
#' library(tidyr)
#'
#' # Join duration variable to sale data
#' sale_unranked <- select(lic, lic_id, duration) %>%
#'     right_join(sale) %>%
#'     select(cust_id, year, duration)
#'
#' # Test Ranking
#' sale_ranked <- rank_sale(sale_unranked)
#' check_rank_sale(sale_ranked, sale_unranked)
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

#' Check license history (stored in a list)
#' @param x list: license history tables returned by \code{\link{make_track}}
#' @import dplyr
#' @family functions for checking license history summarization
#' @export
#' @examples
#' check_track()
check_track <- function(x) {
    y <- list()
    for (i in seq_along(x)) {
        yr <- names(x)[i]
        y[[i]] <- count(x[[i]], duration, left) %>% mutate(year = yr)
    }
    rbind_all(y) %>% tidyr::spread(year, n, fill = "") %>% data.frame()
}

#' Check license history creation
#' @param track list: license history tables returned by \code{\link{make_track}}
#' @param sale data frame: sales table used to make license history
#' @import dplyr
#' @family functions for checking license history summarization
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
#' @family functions for checking license history summarization
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
    rbind_all(y) %>% tidyr::spread(year, n, fill = "") %>% data.frame()
}

#' Check privilege table (R3)
#' @param x list: license privilege tables returned by \code{\link{make_priv}}
#' @import dplyr
#' @family functions for checking license history summarization
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
    rbind_all(y) %>% tidyr::spread(year, n, fill = "") %>% data.frame()
}

#' Check Privilege Table Creation
#' @param priv list: license privilege tables returned by \code{\link{make_priv}}
#' @import dplyr
#' @family functions for checking license history summarization
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

#' Check Final Privilege Table Creation
#' @param priv data frame: license privilege tables returned by
#' @param rnd numeric: number of decimals to round pct change results
#' \code{\link{make_priv_final}}
#' @import dplyr
#' @family functions for checking license history summarization
#' @export
#' @examples
#' check_make_priv_final()
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
