# identify county using bulk mailer & update production data

library(tidyverse)
library(DBI)


## 1. Pull addresses for county identification
f <- "E:/SA/Data-sensitive/Data-Dashboards/SC/standard_deduped.sqlite3"
con <- dbConnect(RSQLite::SQLite(), f)
dbListTables(con)
cust <- tbl(con, "cust") %>%
    filter(cust_res == 1) %>%
    select(cust_id, first, last, street = addr, city, state, zip) %>%
    collect()
dbDisconnect(con)
nrow(cust) == length(unique(cust$cust_id)) # check

dir <- "E:/SA/Data-sensitive/Data-Dashboards/SC/geocode-addresses"
dir.create(dir)
write_csv(cust, file.path(dir, "cust-in.csv"), na = "")


## 2. Identify county
# This is a manual step done using Address Correction in Bulk Mailer

## 3. Append to production data
county <- read_csv(file.path(dir, "cust-out.csv"))
names(county) <- c("cust_id", "county_fips", "county_name")

f <- "E:/SA/Data-production/Data-Dashboards/SC/license.sqlite3"
con <- dbConnect(RSQLite::SQLite(), f)
cust <- dbReadTable(con, "cust")
cust_updated <- left_join(cust, county, by = "cust_id")

# check distribution - compare to census data
f <- "E:/SA/Data-production/Data-Dashboards/_Shared/census.sqlite3"
db2 <- dbConnect(RSQLite::SQLite(), f)
pop <- tbl(db2, "pop_acs") %>%
    filter(state_abbrev == "SC", year == 2017) %>%
    collect() %>%
    group_by(county_fips) %>%
    summarise(n = sum(pop)) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
dbDisconnect(db2)

filter(cust_updated, cust_res == 1) %>%
    count(county_fips, county_name) %>%
    mutate(pct_lic = round(n / sum(n) * 100, 1)) %>%
    right_join(select(pop, county_fips, pct)) %>%
    View()

# update production data
cust_updated <- cust_updated %>%
    select(cust_id, sex, birth_year, cust_res, county_fips, raw_cust_id, cust_period)
dbWriteTable(con, "cust", cust_updated, overwrite = TRUE)
tbl(con, "cust") # check
dbDisconnect(con)
