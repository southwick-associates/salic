
- update sample data, at least remove references to GA
    + make sure it is consistent with current database rules (see summary data documentation)
    + remove description from lic
    + change "priv" to "type" for lic (this impacts validate.R line 218). Need to consider this for other changes as well
    + add lic_res to lic
    + drop extra lic_ids from lic
    + follow type guidelines for variables
    + probably drop state from cust in favor of cust_res
    + add res to sale
- finish implementing first_month()
- Complete Rmd templates
- Write vignette
- add dashboarding calculations to salic
    + maybe drop certain existing functions as part of this? (probably can't remove calc_churn without breaking the churn summary, but I might need to rename it)
