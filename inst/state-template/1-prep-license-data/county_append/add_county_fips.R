#Chelsea Liddell
#3/28/19

#Taking customer data from finalsave and adding
#county codes from bulk mailer

#NOTE
#Before running this, take the .csv from 3-dedup-sale
#and run bulk mailer to add county codes

rm(list = ls())
.libPaths("E:/Program Files/R/R-3.5.1/project-library/data-dashboards")
library(tidyverse)
library(RSQLite)
library(salic)
library(Hmisc)

#some vars - ideally these would get passed from _run.R - maybe someday
#need to both change these two and those in set_params.R
state <- "OR"
period <- "2018-q4" #current quarter update

setwd(paste("E:/SA/Data-sensitive/Data-Dashboards/",state,sep = ""))
codepath <- paste("../../../Projects/Data-Dashboards/",state,"/",period,"/1-prep-license-data",sep = "")

#get variables defined for this state/update, source code for fcns
source(paste(codepath,"/state_spec_fcns/set_params.R",sep =""))

#connect to standard db
conn <- dbConnect(SQLite(),paste("../../../Data-production/Data-Dashboards/",state,"/",db_license,sep = ""))

cust <- dbReadTable(conn,"cust")

#get county codes
ccodes <- read_csv("geocode-addresses/cust_counties.txt",col_types = "ci",progress = F)
names(ccodes)[2] <- "county_fips"

#merge
cust <- left_join(cust,ccodes)

#write
dbWriteTable(conn,"cust",cust,overwrite = T)
dbDisconnect(conn)

