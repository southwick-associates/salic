
library(tidyverse)
library(salic)

source("1-prep-license-data/func.R")
source("1-prep-license-data/func-db.R")

first <- TRUE # first round? (i.e., not an update)


# 1. Raw ------------------------------------------------------------------

sale <- read_sale("path_to_file")
test_overall(sale) # find problems fast
write_raw(sale)
rm(sale)

lic <- read_lic("path_to_file")
write_raw(lic)
rm(lic)

cust <- read_cust("path_to_file")
write_raw(cust)
rm(cust)


# 2. Standardize ----------------------------------------------------------

### a. sale
sale_raw <- load_raw("sale", "cols_to_select")
sale <- standardize_sale(sale_raw)
test_sale(sale, sale_raw)
if (first) build_standard_sale()
write_standard(sale)
rm(sale, sale_raw)

### b. cust
cust_raw <- load_raw("cust", "cols_to_select")
cust <- standardize_cust(cust_raw)
test_cust(cust, cust_raw)
if (first) build_standard_cust()
write_standard(cust)
rm(cust, cust_raw)


# 3. Finalize -------------------------------------------------------------

### a. Add sale transaction month
sale <- load_standard("sale")

### b. Finalize Custoemrs
# - drop those not in sales
# - get birth_year

### c. Identify Sales Residency

### d. Write to sqlite
# also, only writing new (or updated) records
# - this can be done with the sqlite REPLACE statement
if (first) build_license_sale()
write_final(sale)

if (first) build_license_cust()
write_final(cust)

