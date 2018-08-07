# run all

run_dash <- function(
    priv_nm, priv_ref = "NONE", yrs = "2008:2018", res_filter = "NONE", res_type = "NONE", 
    path = "4-dashboard-results/by-priv-results.Rmd" # location of Rmd file
) {
    output_file = paste0("by-priv-results-", priv_nm, ".html")
    rmarkdown::render(
        path, params = list(priv_nm = priv_nm, priv_ref = priv_ref, 
                            res_filter = res_filter, res_type = res_type, yrs = yrs),  
        output_file = output_file
    )
}


# Create Tables by Priv ---------------------------------------------------

# Overall Types
run_dash("boat", yrs = "2010:2018") # first full year on account of 3-year licenses
run_dash("hunt")
run_dash("fish")
run_dash("all_sports")

### Residency-specific
# 1. make reference privs to identify target population (for calculating priv rates)
run_dash("hunt", res_filter = "res")
run_dash("fish", res_filter = "res")
run_dash("hunt", res_filter = "nonres")
run_dash("fish", res_filter = "nonres")

# 2. estimate residency-specific priv results
run_dash("sportsmans", "all_sports", res_type = "res")

run_dash("trout_res", "fish", res_type = "res")
run_dash("res_state_fw", "fish", res_type = "res")
run_dash("res_5day_fw", "fish", res_type = "res")
run_dash("res_state_hunt", "hunt", res_type = "res")

run_dash("trout_nonres", "fish", res_type = "nonres")
run_dash("nonres_state_fw", "fish", res_type = "nonres")
run_dash("nonres_5day_fw", "fish", res_type = "nonres")
run_dash("nonres_state_hunt", "hunt", res_type = "nonres")
run_dash("nonres_3day_hunt", "hunt", res_type = "nonres")


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

# quick check
head(dat, 10)
summary(dat)
count(dat, segment)
count(dat, group)
count(dat, metric, segment) %>% spread(segment, n)
count(dat, metric, segment, group) %>% spread(segment, n) %>% data.frame()

# provide to Ben for summary
# counts by year for each group (permission)
filter(dat, segment == "All", metric == "participants") %>%
    select(group, year, value) %>%
    spread(year, value) %>%
    write.csv(file = "out/priv-counts.csv")

# number of months may very by group-year
filter(dat, segment == "month", 
       year == 2017, metric == "participants") %>% View()

# save output
write.csv(dat, file = "out/dash-out.csv", row.names = FALSE)
