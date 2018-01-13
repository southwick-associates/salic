# Shared functions


# Priv Category Labeling --------------------------------------------------

# basically just calling as.factor() with default labels

factor_age <- function(
    x, levels = 1:7, labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
) {
    factor(x, levels = levels, labels = labels)
}

factor_sex <- function(
    x, levels = 1:2, labels = c("Male", "Female")
) {
    factor(x, levels = levels, labels = labels)
}

factor_res <- function(
    x, levels = c(1,0), labels = c("Resident", "Nonresident")
) {
    factor(x, levels = levels, labels = labels)
}

factor_R3 <- function(
    x, levels = 1:4, labels = c("Carry", "Retain", "Reactivate", "Recruit")
) {
    factor(x, levels = levels, labels = labels)
}

# Priv Category Labeling on data frame ------------------------------------

# similar to the factor_xxx functions above (but for data frames)
# this is helpful for producing automatic checks of the recoding operation

df_factor_sex <- function(df, levels = 1:2, labels = c("Male", "Female")) {
    df <- df %>% mutate(
        sex_old = sex,
        sex = factor(sex, levels = levels, labels = labels)
    )
    count(df, sex, sex_old) %>% data.frame() %>% print()
    select(df, -sex_old)
}

df_factor_res <- function(df, levels = c(1,0), labels = c("Resident", "Nonresident")) {
    df <- df %>% mutate(
        res_old = res,
        res = factor(res, levels = levels, labels = labels)
    )
    count(df, res, res_old) %>% data.frame() %>% print()
    select(df, -res_old)
}

df_factor_R3 <- function(df, levels = 1:4, 
                         labels = c("Carry", "Retain", "Reactivate", "Recruit")) {
    df <- df %>% mutate(
        R3_old = R3,
        R3 = factor(R3, levels = levels, labels = labels)
    )
    count(df, R3, R3_old) %>% data.frame() %>% print()
    select(df, -R3_old)
}

df_factor_age <- function(
    df, levels = 1:7,  
    labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+") 
) {
    df <- df %>% mutate(
        age_old = age,
        age = factor(age, levels = levels, labels = labels)
    )
    count(df, age, age_old) %>% data.frame() %>% print()
    select(df, -age_old)
}

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

