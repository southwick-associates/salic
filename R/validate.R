# functions for validating state license data

#' Summarize sales and customers by year
#' @param x data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @param include_revenue logical: If TRUE, revenue by year will also be shown
#' @param out character: file path to optional output csv
#' @param title character: title for output table
#' @param note character: note for output table
#' @inheritParams pct_round
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' load(sale, lic, package = "salic")
#' 
#' summary_sale(sale)
#' summary_sale(sale, out = "summary_sale.csv")
#'
#' sale <- left_join(sale, lic)
#' filter(sale, priv == "hunt") %>%
#'     summary_sale(title = "Hunting Sales/Cust", note = "An additional note")
#'
#' # example with made-up license revenue
#' sale <- mutate(sale, revenue = 30)
#' summary_sale(sale, rnd = 2, include_revenue = T)
summary_sale <- function(x, include_revenue = FALSE, rnd = 1, out = NULL,
                         title = "Summarize Sales & Customers", note = NULL) {
    pct_round2 <- function(x) pct_round(x, rnd)
    cust <- select(x, cust_id, year) %>% distinct %>% count(year) %>%
        mutate(change_cust = (n - lag(n)) / lag(n)) %>%
        rename(customers = n)
    sale <- count(x, year) %>%
        mutate(change_sales = (n - lag(n)) / lag(n)) %>%
        rename(sales = n)
    if (include_revenue) {
        if (!("revenue" %in% colnames(x))) {
            stop(paste("The input data frame must have a column called 'revenue'",
                       "if include_revenue = TRUE"), call. = FALSE)
        }
        revenue <- group_by(x, year) %>%
            summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
            mutate(change_revenue = (revenue - lag(revenue)) / lag(revenue))
        output <- left_join(cust, sale, by = "year") %>%
            left_join(revenue, by = "year") %>%
            select(year, change_cust, customers, change_sales, sales,
                   change_revenue, revenue)
        out_print <- mutate_at(output, vars(change_cust, change_sales, change_revenue),
                               funs(pct_round2))
    } else {
        output <- left_join(cust, sale, by = "year") %>%
            select(year, change_cust, customers, change_sales, sales)
        out_print <- mutate_at(output, vars(change_cust, change_sales),
                               funs(pct_round2))
    }
    if (!is.null(out)) write.csv(output, file = out, row.names = FALSE)
    note_out <- "https://wsfrprograms.fws.gov/Subpages/LicenseInfo/LicenseIndex.htm"
    if (!is.null(note)) note_out <- paste0(note_out, ")\n(", note)
    print_dat(out_print, title, note_out)
}

#' Summarize churn by year
#'
#' This is a simple calculation that doesn't take into account multi-year
#' licenses, so it should only be used for initial data validation
#' @param x data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @param years numeric: range of years needed to calculate churn
#' @inheritParams summary_sale
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' load(sale, lic, package = "salic")
#' 
#' summary_churn(sale, 2004:2013)
#' summary_churn(sale, 2004:2013, out = "summary_churn.csv")
#'
#' library(dplyr)
#' sale <- left_join(sale, lic)
#' filter(sale, priv == "hunt") %>%
#'     summary_churn(2004:2013, title = "Hunting Churn", note = "An additional note")
summary_churn <- function(x, years, rnd = 1, out = NULL,
                          title = "Churn by Year", note = NULL) {
    # get a single row per customer-year for churn calculation
    y <- select(x, cust_id, year) %>%
        filter(year %in% years) %>%
        distinct()
    churn <- vector(mode = "numeric", length = length(years) - 1)
    yrs <- vector(mode = "character", length = length(years) - 1)
    for (i in seq_along(years[-1])) {
        churn[i] <- calc_churn(y, years[i+1]) * 100
        yrs[i] <- years[i+1]
    }
    output <- data.frame(year = yrs, churn)
    out_print <- mutate(output, churn = pct_round(churn, rnd, scale = 1))
    if (!is.null(out)) write.csv(output, file = out, row.names = FALSE)
    note_out <- "typical hunting: 20-45% --- typical fishing: 35-60%"
    if (!is.null(note)) note_out <- paste0(note_out, ")\n(", note)
    print_dat(out_print, title, note_out)
}

#' Calculate churn for a single year
#'
#' This is a simple calculation that doesn't take into account multi-year
#' licenses, so it should only be used for initial data validation
#' @param x data frame: Table holding sales by year
#' @param year numeric: year to calculate churn (e.g., churn for 2014 uses sales
#' for 2 years: 2013 and 2014)
#' @import dplyr
#' @family functions for validating license data
#' @return Returns a numeric vector of length 1
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' load(sale, lic, package = "salic")
#' 
#' y <- select(sale, cust_id, year) %>%
#'     filter(year %in% 2012:2013) %>% distinct()
#'
#' calc_churn(y, 2013)
calc_churn <- function(x, yr) {
    y1 <- filter(x, year == yr-1)
    y2 <- filter(x, year == yr)
    renew <- inner_join(y1, y2, by = "cust_id")
    # churn calculation
    held_license_y1 <- nrow(y1)
    didnt_renew <- held_license_y1 - nrow(renew)
    didnt_renew / held_license_y1
}

#' Summarize license sales by selected grouping variables
#' 
#' Note: This function is deprecated. I recommend using dpylr/tidyr instead (see examples).
#' @param x data frame: Table holding sales by year
#' @param groups character: grouping variables to be used for the summary
#' @param show_change logical: If TRUE, include a table showing percent changes
#' by year
#' @inheritParams summary_sale
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' load(sale, lic, package = "salic")
#' 
#' # I recommend just using dplyr/tidyr (count-spread) rather than lic_summary()
#' x <- select(lic, lic_id, description) %>% left_join(sale)
#' count(x, description) %>% spread(year, n) %>% View()
#'
#' lic_summary(x, c("lic_id", "description"))
#' lic_summary(x, c("lic_id", "description"), show_change = T)
#' lic_summary(x, c("lic_id", "description"), out = "lic_summary.csv")
#'
#' lic_summary(x, c("lic_id", "description"), title = "Title", note = "Note")
#' lic_summary(x, c("lic_id", "description"),
#'     show_change = T, title = "Title", note = "Note")
lic_summary <- function(x, groups, show_change = FALSE, out = NULL,
                        title = "License Counts by Year", note = NULL) {
    y <- filter(x, !is.na(year)) %>%
        count_(c("year", groups)) %>% ungroup()
    output <- tidyr::spread(y, year, n, fill = "")
    print_dat(output, title, digits = 0, big.mark = "", note = note)
    if (show_change) {
        cat("\n")
        group_by_(y, groups) %>% arrange_(groups) %>%
            mutate(change = round((n - lag(n)) / lag(n) * 100, 0),
                   change = ifelse(is.na(change), "", change)) %>%
            select(-n) %>% tidyr::spread(year, change, fill = "") %>%
            print_dat(paste(title, "Percent Change"),
                          digits = 0, big.mark = "", note = note)
    }
    if (!is.null(out)) write.csv(output, file = out, row.names = FALSE)
}

#' Summarize license revenue by selected grouping variables
#' @param x data frame: Table holding sales by year
#' @param groups character: grouping variables to be used for the summary
#' @param show_change logical: If TRUE, include a table showing percent changes
#' by year
#' @inheritParams summary_sale
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' load(sale, lic, package = "salic")
#' 
#' x <- select(lic, lic_id, description) %>% left_join(sale) %>% mutate(revenue = 30)
#'
#' lic_summary_revenue(x, c("lic_id", "description"))
#' lic_summary_revenue(x, c("lic_id", "description"), out = "lic_summary_rev.csv")
#'
#' lic_summary_revenue(x, c("lic_id", "description"), title = "Title", note = "Note")
lic_summary_revenue <- function(x, groups, show_change = FALSE, out = NULL,
                                title = "Revenue Sum by Year", note = NULL) {
    y <- filter(x, !is.na(year)) %>%
        group_by_(.dots = c("year", groups)) %>%
        summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
        ungroup()
    output <- mutate(y, revenue = round(revenue, 0)) %>%
        tidyr::spread(year, revenue, fill = 0)
    print_dat(output, title, digits = 0, big.mark = "", note = note)
    if (show_change) {
        cat("\n")
        group_by_(y, groups) %>% arrange_(groups) %>%
            mutate(change = round((revenue - lag(revenue)) / lag(revenue) * 100, 0),
                   change = ifelse(is.na(change), "", change)) %>%
            select(-revenue) %>% spread(year, change, fill = "") %>%
            print_dat(paste(title, "Percent Change"),
                              digits = 0, big.mark = "", note = note)
    }
    if (!is.null(out)) write.csv(output, file = out, row.names = FALSE)
}

#' Summarize customers by year for a specified demographic
#'
#' Note - this needs work to provide more info and the option for csv output
#' @param x data frame: Table holding sales by year
#' @param groups character: grouping variables to be used for the summary
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' cust_summary_demo()
cust_summary_demo <- function(x, groups) {
    y <- distinct(x)
    calc <- filter(y, !is.na(year)) %>%
        count_(c("year", groups))
    # counts
    ungroup(calc) %>% tidyr::spread(year, n, fill = 0) %>% printf()
    cat("\n")
    # proportions
    mutate(calc, pct = round(n / sum(n) * 100, 1) %>% paste0("%")) %>%
        select(-n) %>% ungroup() %>%
        tidyr::spread(year, pct, fill = "0%") %>%
        printf()
}
