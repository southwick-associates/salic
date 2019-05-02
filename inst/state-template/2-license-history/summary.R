#' ---
#' title: "Run permission summaries (customer counts overall and by demographic)"
#' output: 
#'     html_document:
#'         code_folding: hide
#' ---

library(tidyverse)
library(DBI)
library(salic)


# Params (for Testing) -------------------------------------------------------

# These will be suppressed in production runs: via run_summary()
# (i.e., params_passed variable should only exist if script is sourced from a function call)
if (!exists("params_passed")) {
    data_src <- "db_history"
    firstyr <- 2009
    lastyr <- 2018
    db_license <- "E:/SA/Data-production/Data-Dashboards/__state__/license.sqlite3"
    db_history <- "E:/SA/Data-production/Data-Dashboards/__state__/history.sqlite3"
}


# Get Permission Counts ---------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), db_license)
perm <- dbReadTable(con, "permission")
cust <- tbl(con, "cust") %>% select(cust_id, sex, birth_year) %>% collect()

if (data_src == "db_license") {
    
    lic <- tbl(con, "lic") %>% select(lic_id, duration) %>% collect()
    perm <- left_join(perm, lic, by = "lic_id")
    sale <- tbl(con, "sale") %>% select(cust_id, lic_id, year, res) %>% collect()
    
    count_perm <- function(priv_name) {
        lic_slct <- filter(perm, permission == priv_name) %>%
            select(lic_id, duration)
        hist <- sale %>%
            right_join(lic_slct, by = "lic_id") %>%
            filter(year %in% firstyr:lastyr) %>%
            group_by(cust_id, year) %>%
            summarise(res = max(res), duration = max(duration)) %>% 
            ungroup()
        # customer counts
        hist %>%
            left_join(cust, by = "cust_id") %>%
            recode_agecat() %>%
            count(year, res, sex, agecat, duration) %>%
            mutate(permission = priv_name)
    }
    # count_perm("hunt")
    
    counts <- lapply(unique(perm$permission), count_perm) %>% bind_rows()
    rm(sale, cust)
} 
dbDisconnect(con)

if (data_src == "db_history") {
    
    con <- dbConnect(RSQLite::SQLite(), db_history)
    
    count_perm <- function(priv_name) {
        hist <- tbl(con, priv_name) %>%
            select(cust_id, year, duration = duration_run, res, R3, lapse) %>%
            collect()
        hist %>%
            left_join(cust, by = "cust_id") %>%
            recode_agecat() %>%
            count(year, res, sex, agecat, duration, R3, lapse) %>%
            mutate(permission = priv_name)
    }
    # count_perm("hunt")
    
    counts <- lapply(unique(perm$permission), count_perm) %>% bind_rows()
    rm(cust)
    dbDisconnect(con)
}


# Run Summaries -----------------------------------------------------------

## Overall
counts %>%
    mutate(duration = factor(duration)) %>%
    group_by(permission, year, duration) %>%
    summarise(n = sum(n)) %>%
    ggplot(aes(year, n, fill = duration)) +
    geom_col() +
    facet_wrap(~ permission, scale = "free_y") +
    ggtitle("Overall Cust Counts", "For db_history, duration refers to running duration")


## By Demographic
pct_calc <- function(df, var) {
    df %>%
        group_by_("permission", "year", var) %>%
        summarise(n = sum(n)) %>%
        mutate(pct = n / sum(n))
}

# Age categories
yrs <- firstyr:lastyr
age_yrs <- c(min(yrs), round(median(yrs)), max(yrs))

counts %>%
    filter(year %in% age_yrs) %>%
    pct_calc("agecat") %>%
    ggplot(aes(agecat, pct, fill = agecat)) +
    geom_col() +
    facet_grid(year ~ permission) +
    scale_y_continuous(labels=scales::percent) +
    theme(axis.text.x = element_blank(), legend.position = "bottom") +
    ggtitle("Distributions - Age Category")

# Minority gender & residency
pct_plot <- function(df, var) {
    df %>%
        ggplot(aes_string("year", "pct", fill = var)) +
        geom_col() +
        facet_wrap(~ permission) +
        scale_y_continuous(labels=scales::percent)
}

counts %>%
    df_factor_sex() %>%
    pct_calc("sex") %>%
    filter(is.na(sex) | sex == "Female") %>%
    pct_plot("sex") +
    ggtitle("Non-Male Percentages")

counts %>%
    df_factor_res() %>%
    pct_calc("res") %>%
    filter(is.na(res) | res == "Nonresident") %>%
    pct_plot("res") +
    ggtitle("Non-Resident Percentages")

# R3
if (data_src == "db_history") {
    counts %>%
        df_factor_R3() %>%
        pct_calc("R3") %>%
        pct_plot("R3") +
        ggtitle("R3 Percentages")
}

# Churn
if (data_src == "db_history") {
    counts %>%
        group_by(permission, year) %>%
        summarise(churn = mean(lapse)) %>%
        ggplot(aes(year, churn)) +
        geom_line() +
        facet_wrap(~ permission) +
        ggtitle("Churn")
}
