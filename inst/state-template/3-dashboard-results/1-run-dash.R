# Run dashboard results for each Permission

library(tidyverse)
library(salic)
library(DBI)
source("3-dashboard-results/func.R")


# Define Params -----------------------------------------------------------

state <- "__state__"
yrs <- 2009:2018
dashboard_yrs <- max(yrs) # focus years to be available in dashboard dropdown menu
quarter <- 4 # quarter to be estimated
current_quarter <- 4 # quarter of most recent data
data_dir <- "E:/SA/Data-production/Data-Dashboards"
out_dir <- "data/3-dashboard-results"
script_dir <- "3-dashboard-results"


# Pull Customer Data --------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), file.path(data_dir, state, "license.sqlite3"))
cust <- tbl(con, "cust") %>% select(cust_id, sex, birth_year, county_fips) %>% collect()
dbDisconnect(con)


# Build Tableau Data -------------------------------------------------------

# for running parameterized dash-permission.R
run_dash <- function(priv_nm, priv_ref = "NONE") {

    params_passed <- TRUE # to disable default script parameters
    out_nm <- c(priv_nm, max(yrs), current_quarter) %>% paste(collapse = "-")
    dir.create(file.path(script_dir, "log"), showWarnings = FALSE)
    
    rmarkdown::render(
        input = file.path(script_dir, "dash-permission-annual.R"),
        output_file = file.path("log", paste0(out_nm, ".html")),
        knit_root_dir = getwd(), quiet = FALSE
    )
}

run_dash("hunt")
run_dash("fish")
run_dash("all_sports")

# run_dash("freshwater", "fish")
# run_dash("saltwater", "fish")


# Stack & Summarize -------------------------------------------------------

# TODO: maybe extend/formalize the summaries that get produced here

dat <- out_dir %>%
    list.files(full.names = T) %>%
    grep("*.rds", ., value = T) %>%
    lapply(readRDS) %>%
    bind_rows()

# check row counts
# - first year won't have churn
# - first 5 years won't have recruitment
filter(dat, segment != "month") %>%
    count(group, year) %>%
    spread(year, n)

# provide to Ben for summary
# counts by year for each group (permission)
filter(dat, segment == "All", metric == "participants") %>% 
    select(group, year, value) %>% 
    spread(year, value) %>%
    write.csv(file = "out/priv-counts.csv")

# save output
write.csv(dat, file = "out/dash-out.csv", row.names = FALSE, na = "")
