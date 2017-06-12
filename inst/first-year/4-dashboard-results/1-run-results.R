# run all

run_dash <- function(
    priv_nm, priv_ref = "NONE", yrs = "2007:2016", res_filter = "NONE", res_type = "NONE", 
    path = "code/6-dashboard-results/by-priv-results.Rmd" # location of Rmd file
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

# Privs/subtypes that aren't residency-specific
run_dash("trapping", "hunt")
run_dash("small_game", "hunt")
run_dash("migratory_bird", "hunt")
run_dash("archery_deer_turkey", "hunt")
run_dash("firearm_deer", "hunt")
run_dash("firearm_spring_turkey", "hunt")
run_dash("firearm_fall_turkey", "hunt")

### Privs/subtypes that are residency-specific
# 1. make reference privs to identify target population (for calculating priv rates)
run_dash("hunt", res_filter = "nonres")
run_dash("all_sports", res_filter = "res")

# 2. estimate residency-specific priv results
run_dash("daily_small_game", "hunt", res_type = "nonres")
run_dash("sg_and_fish", "all_sports", res_type = "res")


# Stack Tables for Tableau Input ------------------------------------------

library(dplyr)
library(tidyr)

# load
rdata_dir <- "data/6-dashboard-results"
files <- list.files(rdata_dir, pattern = "\\.RDATA$")
file_paths <- file.path(rdata_dir, files)
dat <- list()
for (i in seq_along(file_paths)) {
    load(file_paths[i])
    dat[[i]] <- out_tbl
}
dat <- bind_rows(dat)

# note: residency-specific permissions
filter(dat, group == "daily_small_game", segment == "Residency") %>% View()
filter(dat, group == "sg_and_fish", segment == "Residency") %>% View()

# quick check
head(dat)
summary(dat)
count(dat, segment)
count(dat, group)
count(dat, metric, segment) %>% spread(segment, n)
count(dat, group, metric, segment) %>% spread(segment, n) %>% data.frame()

# number of months may very by group-year
filter(dat, group == "all_sports", segment == "month", 
       year %in% c(2015, 2015), metric == "participants") %>% View()

# save output
write.csv(dat, file = "out/dash-out.csv", row.names = FALSE)
