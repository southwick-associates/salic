# Get documenation needed for dashboard

# Documentation (docx) to be updated for each dashboard. 
# - To be stored in the O365 group: __state__ > Deliverables
# - Template in the O365 group: Analyst Docs > Docs to Share with States > XX Method...docx

# - Duplication Summary (included in Chelsea's documentation.pdf, stored in the 1-prep-license-data folder)
# - License Types: A stripped-down table to include in the summary
# - Other state-specific info as needed (e.g., lifetime licenses, etc.)


library(tidyverse)

## License Types
db <- src_sqlite("E:/SA/Data-production/Data-Dashboards/__state__/license.sqlite3")
lic <- tbl(db, "lic") %>%
    select(lic_id, description, type, priv) %>%
    collect()
write_csv(lic, "4-methods-summary/lic-docx.csv", na = "")
