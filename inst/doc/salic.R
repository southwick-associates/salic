## ----setup, include = FALSE, message = FALSE-----------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)
library(dplyr)
library(salic)

## ------------------------------------------------------------------------
library(salic)
library(dplyr)

## ------------------------------------------------------------------------
# load sample data
data(cust, lic, sale) 

# prints no warnings
data_check(cust, lic, sale) 

# introduce rule-breaking changes
cust_tmp <- cust
cust_tmp$birth_year <- NULL
cust_tmp$sex[1] <- 3
data_check(cust_tmp, lic, sale)

## ------------------------------------------------------------------------
# check customer ID
length(unique(cust$cust_id)) == nrow(cust)

# check license ID
length(unique(lic$lic_id)) == nrow(lic)

## ------------------------------------------------------------------------
count(cust, sex)

## ------------------------------------------------------------------------
# sales by month in 2016
filter(sale, year == 2016) %>%
    count(year, month)

## ------------------------------------------------------------------------
# count of customers (by year) who purchased in the first 6 months
filter(sale, year > 2015, month <= 6) %>%
    distinct(cust_id, year) %>%
    count(year)

## ------------------------------------------------------------------------
count(lic, type)

# count hunters for select years
filter(lic, type %in% c("hunt", "combo")) %>%
    inner_join(sale, by = "lic_id") %>%
    filter(year > 2015) %>%
    distinct(cust_id, year) %>%
    count(year)

## ------------------------------------------------------------------------
count(lic, duration)

# customers who buy 3-year licenses
cust_3yr <- filter(lic, duration == 3) %>%
    inner_join(sale, by = "lic_id") %>%
    filter(year > 2015) %>%
    distinct(cust_id, year)
count(cust_3yr, year)

## ------------------------------------------------------------------------
# all hunting (non-combo) sales in 2016
sale_2016 <- inner_join(
    filter(sale, year == 2016),
    filter(lic, type == "hunt"),
    by = "lic_id"
)
count(sale_2016, duration, year)

# ranked hunting (non-combo) sales in 2016
rank_sale(sale_2016) %>%
    count(duration, year)

## ------------------------------------------------------------------------
# rank all sales
sale_ranked <- inner_join(sale, lic, by = "lic_id") %>%
    rank_sale()

# count customers who purchased licenses
filter(sale_ranked, year %in% 2008:2010) %>%
    count(year)

# count customers who held licenses
make_history(sale_ranked, 2008:2018) %>%
    filter(year %in% 2008:2010) %>%
    count(year)

## ------------------------------------------------------------------------
# no residency information is included in the output by default
history <- make_history(sale_ranked, 2008:2018)
names(history)

# include residency
history <- make_history(sale_ranked, 2008:2018, carry_vars = "res")
names(history)

## ------------------------------------------------------------------------
# build hunting privilege history
hunt <- filter(lic, type %in% c("hunt", "combo")) %>%
    inner_join(sale, by = "lic_id") %>%
    rank_sale() %>%
    make_history(2008:2018)
    
# hunters in 2016, what % lapsed (i.e., churned) in 2017? 
filter(hunt, year == 2016) %>%
    count(year, lapse) %>%
    mutate(pct = n / sum(n) * 100)

## ------------------------------------------------------------------------
# hunters in 2016, what % are in each R3 group?
filter(hunt, year == 2016) %>%
    count(R3) %>%
    mutate(pct = n / sum(n) * 100)

## ------------------------------------------------------------------------
filter(hunt, year %in% 2008:2013) %>%
    count(year, R3)

## ------------------------------------------------------------------------
data(history)

history <- history %>%
    label_categories() %>%
    recode_agecat() %>%
    filter(!agecat %in% c("0-17", "65+")) %>%
    select(cust_id, year, res, sex, agecat, R3, lapse)

# view history for 1 customer
filter(history, cust_id == 6120)

## ------------------------------------------------------------------------
# overall
part <- est_part(history, "tot")
filter(part, year > 2015)

# by gender
part_sex <- est_part(history, "sex")
filter(part_sex, year > 2015)

## ------------------------------------------------------------------------
scaleup_part(part_sex, part) %>%
    filter(year > 2015)

## ------------------------------------------------------------------------
filter(history, year > 2015) %>%
    est_churn("tot")

## ------------------------------------------------------------------------
segments <- c("tot", "res", "sex", "agecat")

# store participant counts in a list of length 4 (1 per segment)
part <- sapply(
    segments, 
    function(i) est_part(history, i, test_threshold = 45), 
    simplify = FALSE
)

# print the first 2 rows of each segment table
for (i in segments) {
    head(part[[i]], 2) %>% print()
    cat("\n")
}

## ------------------------------------------------------------------------
# pull overall participants from sample data
data(metrics)
part <- metrics$participants$tot
head(part, 3)

format_result(part, timeframe = "full-year", group = "all_sports") %>%
    head(3)

## ------------------------------------------------------------------------
all_part <- metrics$participants
lapply(all_part, function(x) format_result(x, "full-year", "all_sports")) %>%
    bind_rows()

