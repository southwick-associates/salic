#' ---
#' title: "Create permission tables (hunt, fish, etc.) in history database"
#' output: 
#'     html_document:
#'         code_folding: hide
#' ---

library(tidyverse)
library(DBI)
library(salic)

# Params ------------------------------------------------------------------
# Intended to be commented out for production runs

# priv_nm <- "hunt" # (fish, hunt, all_sports) or (deer, trout, etc.)
# lic_filter <- 'type %in% c("hunt", "combo")'
# yrs <- 2009:2018
# priv_hist <- "NONE" # (NONE, fish, hunt, etc.) > for subtype permissions (otherwise "NONE")
# test <- FALSE # (TRUE, FALSE) > if TRUE, runs a sample & doesn't write output to sqlite
# db_license <- "E:/SA/Data-production/Data-Dashboards/__state__/license.sqlite3"
# db_history <- "E:/SA/Data-production/Data-Dashboards/__state__/history.sqlite3"


# Load Data ---------------------------------------------------------------

## 1. license types
con <- dbConnect(RSQLite::SQLite(), db_license)
lic_all <- tbl(con, "lic") %>% collect()
lic <- filter_(lic_all, .dots = lic_filter)
DT::datatable(lic)

## 2. sales
# The if condition just ensures the logic works on the sqlite backend (== vs %in%)
if (nrow(lic) == 1) {
    sale <- tbl(con, "sale") %>% filter(lic_id == lic$lic_id) 
} else {
    sale <- tbl(con, "sale") %>% filter(lic_id %in% lic$lic_id)
}
sale <- select(sale, cust_id, lic_id, year, res) %>% 
    filter(year %in% yrs) %>%
    collect()
dbDisconnect(con)

## 3. If testing, take a sample of about 10K license holders to speed-up execution
if (test) {
    cust <- distinct(sale, cust_id)
    sample_size <- min(10000, nrow(cust))
    cust <- sample_n(cust, sample_size)
    sale <- semi_join(sale, cust, by = "cust_id")
    rm(cust)
}


# Rank Sales Data ------------------------------------------------------

# Ranking creates one row per customer-year
# - picks highest duration (e.g., a 5-year license would be chosen over a 1-year license)
# - picks Resident (res == 1) over Nonresident (res == 0).
sale <- sale %>%
    left_join(lic, by = "lic_id") %>%
    rank_sale(rank_var = c("duration", "res"), grp_var = c("cust_id", "year"))


# Prepare Permission Data -------------------------------------------------

## 1. Make License History
lic_history <- sale %>%
    select(cust_id, duration, year, res) %>%
    make_lic_history(yrs, carry_vars = "res")

## 2. Identify R3 & Lapse
if (priv_hist == "NONE") { # for most permissions (overall & privilege types)
    
    if (length(unique(lic_history$year)) > 5) { # R3 requires 5 preceeding years of reference data
        lic_history <- identify_R3(lic_history, yrs)
    } else {
        lic_history$R3 <- NA 
    }
    check_identify_R3(lic_history, yrs) %>% 
        knitr::kable(caption = "R3 category by Yrs Since Purchase") %>% print()
    
    lic_history <- identify_lapse(lic_history, yrs)
    check_identify_lapse(lic_history) %>% 
        knitr::kable(caption = "Lapse by Yrs till next Purchase") %>% print()

} else { # for subtype permissions (e.g., spouse license) 
    
    # first get history for specified reference permission, since subtypes use this other
    #   permission for defining R3 and lapse
    con <- dbConnect(RSQLite::SQLite(), db_history)
    priv_hist <- tbl(con, priv_hist) %>% select(cust_id, year, lapse, R3) %>% collect()
    dbDisconnect(con)
    
    # identify R3/lapse (by year) depending on the reference permission
    lic_history <- lic_history %>%
        select(cust_id, year, res) %>%
        inner_join(priv_hist, by = c("cust_id", "year"))
}

## 3. Finalize Format & Check
lic_history <- lic_history %>%
    select(cust_id, year, res, duration, duration_run, lag_duration_run, bought, lapse, R3, res) %>%
    mutate_at(vars(duration_run, lag_duration_run, year, lapse, R3), "as.integer")

check_history_samp(lic_history) %>% 
    knitr::kable(caption = "Sample of a few customers from license history") %>% print()

# This should show TRUE (i.e., all relevant sales are included)
nrow(sale) == semi_join(lic_history, sale, by = c("cust_id", "year")) %>% nrow()


# Write to Sqlite ---------------------------------------------------------

if (!test) {
    out_nm <- stringr::str_replace_all(priv_nm, " ", "_") # ensure sqlite compatibility
    
    ## 1. Permission History Data
    if (!file.exists(db_history)) src_sqlite(db_history, create = TRUE)
    con <- dbConnect(RSQLite::SQLite(), db_history)
    if (out_nm %in% dbListTables(con)) dbRemoveTable(con, out_nm)
    dbWriteTable(con, out_nm, data.frame(lic_history))
    dbDisconnect(con)
    
    ## 2. License Permission Table
    # The idea here is to have a separate table that explicitly identifies all the 
    #   license types that go into a specific permission. This allows a simple join to
    #   associate license types with permissions
    permission <- select(lic, lic_id, description) %>%
        mutate(permission = out_nm) %>%
        select(permission, lic_id, description)
    
    con <- dbConnect(RSQLite::SQLite(), db_license)
    if (!("permission" %in% dbListTables(con))) {
        dbWriteTable(con, "permission", data.frame(permission))
    } else {
        # overwrite selected priv records to insure only the newest is kept
        permission_old <- tbl(con, "permission") %>% 
            collect() %>%
            filter(permission != out_nm)
        permission <- bind_rows(permission, permission_old)
        dbRemoveTable(con, "permission")
        dbWriteTable(con, "permission", data.frame(permission))
    }
    dbDisconnect(con)
    glimpse(lic_history)
    glimpse(permission)
}
sessionInfo()
