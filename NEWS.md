
## Version 2.0

This release corresponded to making salic easily available for state agencies and other partners. It included many added functions and a number of improvements to the codebase.

### Improvements

- Decreased R package dependencies:
    + removed saproj dependency
    + changed lubridate to suggested (recode_month() uses lubridate)
    + tidyr (will see...might want this for the dashboard formatting step)
- Brought code into compliance with R CMD check
- Added test conditions for functions to build history and calculate metrics. This helps ensure that the functions return the expected results.

### Added Functionality

- New functions:
    + join_firt_month() to maek it easier to produce mid-year results
    + dashboard functions (est_part, etc.)
    + others...
    
### Changes

- Moved Southwick code templates to a different location
- Changed sample data to include tables (cust, lic, sale) that more directly tie into the needs of the nat/reg dashboards. Also added more sample data
    + history
    + (maybe) dashboard_metrics (not sure on name)
- Updated examples to reflect new sample data

## Version 1.0/1.1/1.2

The 1.0 release corresponds to the stable version that was shared internally among Southwick Associates in 2018. Subsequent 1.x versions included only minor changes, mostly with regard to template code.


    
