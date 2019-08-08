
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
    
### Backwards Compatibility

Functions are (almost) completely backward compatible with previous 1.x versions.

#### A. Changes that Do Impact Function behavior

- (maybe) certain variables have been dropped from the output of license history funtions. The only purpose of these variables was for use in downstream checks; their calculation now is part of the relevant checking functions:
    + make_lic_history() >> drops ...
    + identify_R3()
    + identify_lapse()
- salic no longer includes functionality to setup new projects with template code. Corresponding functions (and template code) have been moved to saproj:
    + new_dashboard()
    + update_dashboard()

#### B. Changes that Don't Impact Function behavior

- sample data has been changed to more directly tie into the needs of national/regional dashboards
- examples have been updated for most functions


## Version 1.0/1.1/1.2

The 1.0 release corresponds to the stable version that was shared internally among Southwick Associates in 2018. Subsequent 1.x versions included only minor changes, mostly with regard to template code for settingup new dashboard projects.
