
## 1. Preparation

Southwick Associates analysts need to prepare license data received from state agencies; ultimately producing 3 tables (cust, lic, sale) in a standardized/anonymized format. This process involves successive recoding and validation steps, some of which have been generalized by means of the functions included in this file.

## 2. License History

Standardized data provide a direct means for producing license history tables for specified permissions (e.g., fish, hunt, all_sports). This file includes functions for performing that process. The output history tables include 1 row per customer per year a license is held (annual, multi-year, or otherwise). Additional variables (R3, lapse) are also added for identifying customer dynamics over time.

## 3. Dashboard

Dashboards use summary metrics (participation, churn, etc.) specified across a number of segments (gender, residency, etc.). This file includes functions for calculating these metrics using license history as input.

## Dat

This file includes documentation for the package sample data.

## Category Variables

Anonymized data (cust, lic, sale) include category variables stored as integer values (e.g., cust$sex: 1=Male, 2=Female). Functions are included to work with these variables (mostly involved in converting numeric categories to labelled factors).
