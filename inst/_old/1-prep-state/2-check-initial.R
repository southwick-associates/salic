# initial check of state data
# sale summary, churn, license summary

library(dplyr)
library(tidyr)
library(salic)

f <- "D:/SA/Data/"
yrs <- 2005:2015

# Connect to Data
db <- src_sqlite(f) # connect with dplyr
# src_tbls(db)
# tbl(db, "dat") %>% glimpse()

# Pull Sales
sale <- tbl(db, "dat") %>%
    select(custid, licenseyear, approvalcode, description) %>% collect(n = Inf)
new_names <- c("cust_id", "year", "lic_id", "description")
data.frame(names(sale), new_names) %>% print_dat("Standardize column names")
names(sale) <- new_names

# 1. Customers & Sales by Year
summary_sale(sale)

# 2. Churn by Year
summary_churn(sale, yrs)

# 3. License Sales by Year breakouts
lic_summary(sale, c("lic_id"), show_change = TRUE)
lic_summary(sale, c("lic_id", "description"))
# count(sale, lic_id, description) %>% print_dat("License Descriptions by ID")
