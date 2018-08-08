
library(tidyverse)
library(salic)

source("1-prep-license-data/func.R")


# 1. Raw ------------------------------------------------------------------

sale <- read_sale("path_to_file")
check_overall(sale) # find problems fast
write_raw(sale)
rm(sale)

lic <- read_lic("path_to_file")
write_raw(lic)
rm(lic)

cust <- read_cust("path_to_file")
write_raw(cust)
rm(cust)


# 2. Standardize ----------------------------------------------------------

sale <- load_raw("sale", "cols_to_select")
sale <- standardize_sale(sale)
check_sale(sale)
write_standard(sale)
rm(sale)

cust <- load_raw("cust", "cols_to_select")
cust <- standardize_cust(cust)
check_cust(cust)
write_standard(cust)
rm(cust)


# 3. Finalize -------------------------------------------------------------

# a. Add sale transaction month
sale <- load_standard("sale")

# b. Finalize Custoemrs
# - drop those not in sales
# - get birth_year

# c. Identify Sales Residency

# d. Write to sqlite
write_final(sale)
write_final(cust)

