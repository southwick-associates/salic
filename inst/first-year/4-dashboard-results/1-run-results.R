# run all

run_dash <- function(
    priv_nm, priv_ref = "NONE", yrs = "2010:2017", res_filter = "NONE", res_type = "NONE", 
    path = "code/4-dashboard-results/by-priv-results.Rmd" # location of Rmd file
) {
    if (!is.null(priv_ref)) is_priv = TRUE else is_priv = FALSE
    output_file = paste0("by-priv-results-", priv_nm, ".html")
    rmarkdown::render(
        path, params = list(priv_nm = priv_nm, priv_ref = priv_ref, 
                            res_filter = res_filter, res_type = res_type, yrs = yrs),  
        output_file = output_file
    )
}


# Create Tables by Priv ---------------------------------------------------

# Overall Types
run_dash("hunt")
run_dash("fish")
run_dash("all_sports")

# check residency
# library(tidyverse)
# db <- src_sqlite("~/data2/GA_Dashboard/lic.sqlite3")
# privs <- tbl(db, "lic_priv") %>% count(priv) %>% pull(priv)
# for (i in 1:length(privs)) tbl(db, privs[i]) %>% count(res) %>% print()
# tbl(db, privs[4]) %>% count(res)
# resident only: sportsmans

# Privs that aren't residency-specific
run_dash("big_game", "hunt")
run_dash("waterfowl", "hunt")
run_dash("trout", "fish")
run_dash("commercial_fishing", "fish")
run_dash("saltwater_info_program", "fish")

# Subtypes that aren't residency-specific
run_dash("combination", "all_sports")
run_dash("short_term", "all_sports")
run_dash("nuisance_wildlife", "hunt")

### Residency-specific
# 1. make reference privs to identify target population (for calculating priv rates)
run_dash("all_sports", res_filter = "res")

# 2. estimate residency-specific priv results
run_dash("sportsmans", "all_sports", res_type = "res")


# Stack Tables for Tableau Input ------------------------------------------

library(tidyverse)

# load
rdata_dir <- "data/4-dashboard-results"
files <- list.files(rdata_dir, pattern = "\\.RDATA$")
file_paths <- file.path(rdata_dir, files)
dat <- list()
for (i in seq_along(file_paths)) {
    load(file_paths[i])
    dat[[i]] <- out_tbl
}
dat <- bind_rows(dat)

# note: residency-specific permissions
filter(dat, group == "sportsmans", segment == "Residency") %>% View()

# quick check
head(dat, 10)
summary(dat)
count(dat, segment)
count(dat, group)
count(dat, metric, segment) %>% spread(segment, n)
count(dat, metric, segment, group) %>% spread(segment, n) %>% data.frame()

# provide to Nick for summary
# counts by year for each group (permission)
filter(dat, segment == "All", metric == "participants") %>%
    select(group, year, value) %>%
    spread(year, value) %>%
    write.csv(file = "out/priv-counts.csv")

# number of months may very by group-year
filter(dat, group == "all_sports", segment == "month", 
       year == 2016, metric == "participants") %>% View()

# save output
write.csv(dat, file = "out/dash-out.csv", row.names = FALSE)
