
## 1. Prepare

Agency data requires initial preparation; ultimately to be stored in 3 tables (cust, lic, sale) in a standardized/anonymized format. This involves successive recoding and validation steps, some of which have been generalized using functions included here.

## 2. License-History

The primary function (make_history) takes standardized data as input. It produces license history tables for specified permissions (e.g., fish), which include 1 row per customer per year a license is held (annual, multi-year, or otherwise). Additional variables (R3, lapse) can also be added for identifying customer dynamics over time.

## 3. Dashboard

The dashboard summary functions take license history tables as input. Dashboards use summary metrics (participation, churn, etc.) specified across a number of segments (gender, residency, etc.). 

## Category-Variables

Category variables are stored as numeric values in standardized data. Functions are included here to work with these variables (mostly involved in converting numeric categories to labelled factors).

## Data-Check

Automated checks to help ensure standardized data is stored in the required format.

## Data-Doc

Documentation for the package sample data.

