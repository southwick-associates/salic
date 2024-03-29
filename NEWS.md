
## Version 2.0.2

Made small non-breaking changes:

- make_history(): removed a cryptic warning. Previously, a warning would display if the input sale table had fewer than 6 years (i.e., no R3 would be calculated). This is expected behavior, and I added a condition to prevent the warning.
- salic.R: All namespace and global variable definitions were placed at the top-level of this file for more tidy Roxygen2 documentation

## Version 2.0.1

Introduced small non-breaking changes to two functions:

- scaleup_part(): more informative threshold error message
- format_result(): slightly more generalized for use by Southwick internally

## Version 2.0.0

Released on 2019-08-26; aimed at sharing with state agencies and other partners. It includes a number of new functions and major changes to the codebase. 

### Additions

- Dashboard summary functions
- Functions to check formatting rules of standardized data (cust, lic, sale)
- Tests for all license-history & dashboard summary functions
- An introduction vignette

### Minor Changes

- Sample data has been changed to more directly tie into the needs of national/regional dashboards
- Examples have been re-written for most functions

### Breaking Changes

- The functions for building license history have been completely re-written to be somewhat simpler, and much more performant. Relevant code based on previous versions is not compatible with 2.0. These functions have also been renamed, so accidental use of old code should produce informative errors quickly.

- Southwick script templates have been removed. These will most likely be included in a future version of the saproj package.

## Version 1.x

The 1.0 release corresponds to the stable version that was shared internally among Southwick Associates in 2018. Subsequent 1.x versions included only minor changes, mostly with regard to template code for setting up new dashboard projects.
