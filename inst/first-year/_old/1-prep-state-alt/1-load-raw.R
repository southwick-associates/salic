# load license data into sqlite database
# (assumes data are separated into files by year)

library(readr)
library(dplyr)
library(stringr)

sep <- ","
yrs <- 2005:2015
source("code/1-prep-state/readr-cols.R")

### load customer data into R
f1 <- "D:/SA/Data/"
f <- paste0(f1, yrs[1], ".txt")

# check data loading
read_lines(f, n_max = 2)
nm_in <- read_lines(f, n_max = 1) %>% strsplit(split = sep, fixed = T) %>% unlist()
nm_in <- str_replace_all(nm_in, " ", "") %>% tolower()
# nm_in[1] <- "custid" # to fix "<U+FEFF>" issue in "UTF-8-BOM" files (e.g., WI data)

# x <- read_delim(f, sep, col_names = nm_in, skip = 1, n_max = 5)
# x <- read_delim(f, sep, na = "NULL", col_names = nm_in, skip = 1, col_types = raw_cols)

# load data
dat <- list()
for (i in seq_along(yrs)) {
    f <- paste0(f1, yrs[i], ".txt")
    dat[[i]] <- read_delim(f, sep, na = "NULL", col_names = nm_in, skip = 1,
                           col_types = raw_cols, progress = FALSE)
}
dat <- bind_rows(dat)
# filter(dat, is.na(custid)) %>% View() # for WI
# dat <- filter(dat, !is.na(custid)) # for WI

### Add a record ID
dat$raw_id <- as.integer(row.names(dat))
select(dat, raw_id) %>% head()
select(dat, raw_id) %>% tail()

### Convert dates to character
date_vars <- c("date_of_birth", "dateofpurchase")
dat <- mutate_at(dat, .cols = date_vars, .funs = "as.character")

### Check
# sample_n(dat, 30) %>% View()
glimpse(dat)

# demographics check - state-specific
count(dat, gender) %>% data.frame()
count(dat, licenseyear) %>% data.frame()
count(dat, state) %>% data.frame()


### write to sqlite db
f1 <- "D:/SA/Data/"
f <- file.path(f1, "raw.sqlite3")
if (!file.exists(f)) db <- src_sqlite(f, create = TRUE)
copy_to(db, dat, temporary = FALSE)

# dataset check
db %>% tbl("dat") %>% dim()
db %>% tbl("dat") %>% glimpse()
