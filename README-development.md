
# Information on Developing/Maintaining salic

There are some procedures that should be followed for changes to this package.

- Hadley's book is a good place to start: https://r-pkgs.org/ 
- also: https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf

Most importantly, you must use git: https://github.com/southwick-associates/salic

## For all Major Commits

- run `devtools::test()` (actually recommended for any changes to code, it only takes a few seconds to run)
- run `devtools::check()`
- run `pkgdown::build_site()` to update docs
- run `devtools::build()` and `devtools::build()` to produce source and binary installers (you can make these available on github by following https://help.github.com/en/articles/creating-releases)


## Git Reference

### Github -https://r-pkgs.org/git.html

### SSH Authentication

I recommend setting up your github account (and computer) to connect using SSH, which seems to make the `git push` go more smoothly:
https://help.github.com/en/articles/connecting-to-github-with-ssh


## R Package Reference

### Object Documentation - http://r-pkgs.had.co.nz/man.html

All objects (functions, data) are documented using Roxygen2. 

- I believe they all include examples with the exception of recode_month(): I don't want state agencies using this function since national/regional dashboards are all intended to use calendar years.

### Testing - https://r-pkgs.org/tests.html

Some of the salic tests are based on comparing to expected results (sample data)

### Checking - https://r-pkgs.org/r-cmd-check.html

Before placing on github, Dan began running the CRAN check using `devtools::check()`. This was successfully run without any errors or warnings. 

Avoiding this note (*no visibile binding for global variable '[some variable]'*) depends upon placing a call to `globalVariables()` in prepare.R. See https://github.com/r-lib/devtools/issues/1714 for reference.

### Vignettes - https://r-pkgs.org/vignettes.html

Making sure the vignette is included in the binary build is a bit of a pain. This is what seems to work (when you need to update the vignette):
    # `devtools::install()` to ensure most recent functions, data, etc.
    + `devtools::buildvignettes()` 
    + `devtools::build(binary = TRUE)` to build the binary with the vignette included
