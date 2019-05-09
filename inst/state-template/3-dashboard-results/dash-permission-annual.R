#' ---
#' title: "Run by-permission Tableau Production for annual views"
#' output: 
#'     html_document:
#'         code_folding: hide
#' ---

library(tidyverse)
library(DBI)
library(salic)
source("3-dashboard-results/func.R")


# Testing ------------------------------------------------------------------

# This will be suppressed in production runs: via run_dash()
# (i.e., params_passed variable should only exist if script is sourced from a function call)
if (!exists("params_passed")) {
    
    state <- "__state__"
    yrs <- 2009:2018
    dashboard_yrs <- max(yrs) # focus years to be available in dashboard dropdown menu
    data_dir <- "E:/SA/Data-production/Data-Dashboards"
    out_dir <- "data/3-dashboard-results"
    
    priv_nm <- "hunt" # (fish, hunt, all_sports) or (deer, trout, etc.)
    priv_ref <- "NONE" # (NONE, fish, hunt, etc.) > subtype/priv permissions (otherwise "NONE")
    out_nm <- c(priv_nm, max(yrs)) %>% paste(collapse = "-")
    
    # pull customer data
    con <- dbConnect(RSQLite::SQLite(), file.path(data_dir, state, "license.sqlite3"))
    cust <- tbl(con, "cust") %>% select(cust_id, sex, birth_year, county_fips) %>% collect()
    dbDisconnect(con)
}


# Get Census Data ------------------------------------------------------
# note: this isn't actually permission-specific, but it runs very quickly

con <- dbConnect(RSQLite::SQLite(), file.path(data_dir, "_Shared/census.sqlite3"))
county_fips <- tbl(con, "county_fips") %>% 
    filter(state_abbrev == state) %>%
    select(county_fips, county = county_name) %>%
    collect()
cust <- left_join(cust, county_fips, by = "county_fips") %>% select(-county_fips)

pop_county <- tbl(con, "pop_acs") %>%
    select(-state) %>% 
    filter(state_abbrev == state, year %in% yrs) %>%
    collect()
dbDisconnect(con)

pop_county <- pop_county %>%
    aggregate_pop() %>% # collapse to 7 age categories
    label_categories() %>% # convert numeric categories to factor
    left_join(county_fips, by = "county_fips") %>%
    extrapolate_pop(yrs) # extrapolate (if needed) with statewide avg % change

group_by(pop_county, year) %>% 
    summarise(sum(pop)) %>% 
    knitr::kable(caption = "State Population", format.args = list(big.mark = ","))


# Get Production Data ----------------------------------------------------
# Note: If subtypes are present, will need additional code (see VA 2018 Q4)

## 1. Permission license history Data
con <- dbConnect(RSQLite::SQLite(), file.path(data_dir, state, "history.sqlite3"))
priv <- tbl(con, priv_nm) %>%
    select(cust_id, year, res, lapse, R3) %>%
    filter(year %in% yrs) %>%
    collect()
priv <- priv %>%
    left_join(cust, by = "cust_id") %>%
    label_categories() %>%
    recode_agecat() %>%
    select(cust_id, year, res, lapse, R3, sex, age = agecat, county)
dbDisconnect(con)

## 2. Sales: for showing purchases by month
con <- dbConnect(RSQLite::SQLite(), file.path(data_dir, state, "license.sqlite3"))
lic <- tbl(con, "permission") %>% 
    filter(permission == priv_nm) %>% 
    collect()
sale <- tbl(con, "sale") %>%
    filter(year %in% yrs, lic_id %in% lic$lic_id) %>%
    select(cust_id, year, month) %>% 
    collect()
dbDisconnect(con)


# Estimate ----------------------------------------------------------------

segs <- c("tot", "res", "sex", "age", "county")

## 1. Participants
part <- sapply(segs[1:4], function(i) est_part(priv, i), simplify = F)
part <- lapply(part, function(x) scaleup_part(x, part$tot))

# Resident Participants 
# (intermediate results for rate estimation, other than for county)
priv_res <- filter(priv, res == "Resident")
tot_res <- filter(part[["res"]], res == "Resident") # for scaling

part_res <- sapply(segs[-2], function(i) est_part(priv_res, i), simplify = F)
part_res <- lapply(part_res, function(x) scaleup_part(x, tot_res))
part[["county"]] <- part_res[["county"]] # county results are resident only

## 2. Participation rate
if (priv_ref == "NONE") {
    pop <- sapply(segs[-2], function(i) est_pop(pop_county, i), simplify = F)
    rate <- mapply(est_rate, part_res, pop, SIMPLIFY = F)
    
    # residency-specific rates are also included for Tableau (so all nonres show zeroes)
    rate[["res"]] <- select(part[["res"]], res, year) %>%
        left_join(select(rate[["tot"]], year, rate), by = "year") %>%
        mutate(rate = ifelse(res == "Nonresident", 0, rate))
    
} else { 
    # estimate a privilege rate instead (based on reference permission participants)
    out_ref <- str_replace(out_nm, priv_nm, priv_ref)
    pop <- readRDS(file.path(out_dir, "part", paste0(out_ref, ".rds"))) %>%
        lapply(function(x) rename(x, pop = part))
    rate <- mapply(est_rate, part, pop, SIMPLIFY = F, MoreArgs = list(flag_rate = 100))
}

## 3. New recruits
priv_new <- filter(priv, !is.na(R3), R3 == "Recruit")
has_recruit <- nrow(priv_new) > 0 # no R3 will be available if < 5 yrs of data

if (has_recruit) {
    part_new <- sapply(segs[1:4], function(i) est_part(priv_new, i), simplify = F)
    part_new <- lapply(part_new, function(x) scaleup_part(x, part_new$tot))
    
    part_new[["county"]] <- priv_new %>%
        filter(res == "Resident", year %in% dashboard_yrs) %>%
        est_part("county", flag_change = 50) %>%
        scaleup_part(filter(part_new$res, res == "Resident"))
}

## 4. churn rate
churn <- sapply(segs, function(i) est_churn(priv, i), simplify = F)

## 5. monthly sales
sale_month <- bind_rows(
    est_month(sale, dashboard_yrs),
    if (has_recruit) sale %>%
        semi_join(priv_new, by = c("cust_id", "year")) %>%
        est_month(dashboard_yrs, "new")
)

## Format for Tableau
tableau <- function(df, metric) format_tableau(df, metric, dashboard_yrs, county_fips)
out_tbl <- bind_rows(
    sale_month,
    lapply(part, tableau, metric = "participants"),
    lapply(rate, tableau, metric = "participation rate"),
    lapply(churn, tableau, metric = "churn"),
    if (has_recruit) lapply(part_new, tableau, metric = "participants - recruited")
) %>%
    mutate(quarter = 4, group = priv_nm) %>%
    select(quarter, group, metric, segment, year, category, value)
glimpse(out_tbl)


# Save ----------------------------------------------------------------------

## save out_tbl results of selected permission
dir.create(out_dir, showWarnings = FALSE)
saveRDS(out_tbl, file.path(out_dir, paste0(out_nm, ".rds")))

## save total participant results for reference permissions (e.g., hunt, fish)
# privileges such as trout will use these for estimating privilege rate
if (priv_ref == "NONE") {
    dir.create(file.path(out_dir, "part"), showWarnings = FALSE)
    saveRDS(part, file.path(out_dir, "part", paste0(out_nm, ".rds")))
}

## check
# Row Counts
# - churn will likely be 10% smaller (except for county)
# - recruited will likely be around 50% smaller (5 yrs can't be counted)
# - month only applies to participants
count(out_tbl, metric, segment) %>% 
    spread(segment, n) %>%
    knitr::kable(caption = "Row counts by metric-segment")

# Summary values - particularly looking for unexpected NAs
options(scipen = 999)
group_by(out_tbl, metric, segment) %>%
    summarise(val = mean(value)) %>%
    spread(segment, val) %>%
    knitr::kable(caption = "Mean values by metric-segment", 
                 digits = 2, format.args = list(big.mark = ","))

sessionInfo()
