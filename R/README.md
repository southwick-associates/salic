
The code is organized into files primarily based on the corresponding workflow, separated into 3 main sections.

## 1. Prepare

Agency data requires initial preparation; ultimately to be stored in 3 tables (cust, lic, sale) in a standardized/anonymized format. This involves successive recoding and validation steps, some of which have been generalized using functions included here.

## 2. License-History

The primary function (make_lic_history) takes standardized data as input. It produces license history tables for specified permissions (e.g., fish, hunt, all_sports), which include 1 row per customer per year a license is held (annual, multi-year, or otherwise). Additional variables (R3, lapse) can also be added for identifying customer dynamics over time.

## 3. Dashboard

The dashboard summary functions take license history tables as input. Dashboards use summary metrics (participation, churn, etc.) specified across a number of segments (gender, residency, etc.). 

## Dat

Documentation for the package sample data.

## Category-Variables

Category variables are stored as numeric values in standardized data. Functions are included here to work with these variables (mostly involved in converting numeric categories to labelled factors).
