# initial check of state data

library(dplyr)
library(tidyr)
library(salic)
library(lubridate)

source("code/1-prep-state/funs.R")
f <- "D:/SA/Data/"
options(width = 200)

### Connect to Data
db <- src_sqlite(f) # connect with dplyr
# src_tbls(db)
# tbl(db, "dat") %>% glimpse()

### Check Customers
# get customer data
cust <- tbl(db, "dat") %>%
    select() %>% collect(n = Inf)
new_names <- c("cust_id", "first", "last", "state", "dob", "sex", "dot")
data.frame(names(cust), new_names) %>% print_dat("Standardize cust column names:")
names(cust) <- new_names

cat_title("Checking customer ID", "distinct - cust_id, dob, last, first")
x <- select(cust, cust_id, dob, last, first) %>% distinct()

# 1. false negatives - one person with multiple IDs
select(x, dob, last, first) %>%
    check_dups("Dups: Customers (dob, last, first) with multiple IDs")
dup <- count(x, dob, last, first) %>% ungroup() %>% filter(n > 1)
if (nrow(dup) > 1) {
    dup$dup_id <- as.integer(row.names(dup))
    dup_all <- left_join(dup, cust, by = c("dob", "last", "first"))
    sample_n(dup, 10) %>% select(dup_id) %>% left_join(dup_all, by = "dup_id") %>%
        arrange(dob, last, first, dot) %>%
        print_dat("Investigate Customers with multiple IDs",
                  "Sample of 10 random customers with multiple IDs in dataset")
}

# 2. False Positives - one ID with multiple people
select(x, cust_id) %>%
    check_dups("Dups: IDs with multiple Customers (dob, last, first)")
dup <- count(x, cust_id) %>% ungroup() %>% filter(n > 1)
if (nrow(dup) > 1) {
    dup$dup_id <- as.integer(row.names(dup))
    dup_all <- left_join(dup, cust, by = "cust_id")
    sample_n(dup, 10) %>% select(dup_id) %>% left_join(dup_all, by = "dup_id") %>%
        arrange(cust_id, dob, last, first, dot) %>%
        print_dat("Investigate IDs with multiple customers",
                  "Sample of 10 random IDs with multiple people in dataset")
}

# check customer demographics
cust <- distinct_rows(cust, "dot", "cust_id")
cust$age <- year(Sys.Date()) - year(cust$dob)
check_fields(cust, c("sex", "state", "age"))


### Check Licenses
# tbl(db, "dat") %>% glimpse()
sale <- tbl(db, "dat") %>%
    select() %>% collect(n = Inf)
new_names <- c("cust_id", "dob", "sex", "year", "lic_id", "description", "dot", "raw_id")
data.frame(names(sale), new_names) %>% print_dat("Standardize sales column names:")
names(sale) <- new_names

lic <- count(sale, lic_id, description)
data.frame(rows = nrow(lic), unique_ids = length(unique(lic$lic_id)),
           unique_descriptions = length(unique(lic$description))) %>%
    print_dat("Unique lic IDs vs unique descriptions")
print_dat(lic, "Table of license ID-description")


### Check Sales
x <- count(sale, lic_id, year)
ungroup(x) %>% spread(year, n, fill = "") %>% print_dat("License Sales by lic_id, year")

x <- count(sale, lic_id, description, year)
ungroup(x) %>% spread(year, n, fill = "") %>%
    print_dat("License Sales by lic_id, description, year")

x <- select(sale, cust_id, year) %>% distinct()
count(x, year) %>% print_dat("Customers by Year")

# Check for duplicate sales cust_id, lic_id
select(sale, cust_id, year, lic_id) %>%
    check_dups("Duplicate sales based on cust_id, year, lic_id")

options(width = 90)
