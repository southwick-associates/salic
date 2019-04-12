# Shared functions

# Dashboard Functions --------------------------------------------------

# formatting column names in output for Tableau
format_metric <- function(x, metric = "participants", val = "n") {
    ungroup(x) %>%
        mutate(metric = metric) %>%
        rename_("value" = val)
}

format_grp <- function(x, seg, cat = NULL) {
    if (is.null(cat)) cat = seg
    x$segment <- seg
    rename_(x, "category" = cat) %>%
        filter(!is.na(category)) # for churn calculation
}

# for grouping, need an additional calculation to deal with missing values
# applying percentage (NA excluded) to total for breakouts
# the input table should be grouped by year
est_total <- function(x, grp, total) {
    x <- x[!is.na(x[[grp]]), ]
    mutate(x, pct = n / sum(n)) %>%
        left_join(total, by = "year") %>%
        mutate(n = pct * n.y,
               pct_diff = (n - n.x) / n.x * 100) %>%
        select(-pct, -n.y)
    # select(-n.x, -pct, -n.y)
}

est_total_check <- function(x) {
    filter(x, !is.na(n.x)) %>%
        mutate(pct_diff = round(pct_diff, 4)) %>%
        # select(year, res, segment, category, pct_diff) %>%
        group_by(year, res, segment) %>%
        summarise(pct_diff = max(pct_diff)) %>%
        spread(year, pct_diff)
}

est_churn <- function(x, grp = NULL) {
    cnt_grp <- c("year", grp, "lapse")
    group_by(x, .dots = cnt_grp) %>%
        summarise(n = n()) %>%
        mutate(churn = n / sum(n)) %>%
        ungroup() %>%
        # since churn in current year uses lapse from previous year
        mutate(year = year + 1) %>%
        filter(lapse == 1) %>%
        select(-n, -lapse)
}


# Visual Dashboard Checking -----------------------------------------------

# grouped line plots with interactive widgets
# not currently implemented

