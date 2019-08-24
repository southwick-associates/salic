# salic: An R package to prepare agency dashboard data

This package includes a set of functions for summarizing agency data, particularly for use in AFWA's national/regional dashboard effort. It's intended to simplify/standardize this process by providing a set of easy-to-use functions.

## Installation

First ensure dependencies are installed: 

- R >= 3.5.0: https://www.r-project.org/
- Two R packages: `install.packages(c("dplyr", "data.table"))`

Salic can be installed from source using devtools: `devtools::install_github("southwick-associates/salic")`. Alternatively, you can use one of the included binary installers:

- Windows: `install.packages("bla bla")`
- Mac: `install.packages("bla bla")`

## Usage

A guide is included in the [Introduction to salic](https://southwick-associates.github.io/salic/articles/salic.html) vignette (also available by running `vignette("salic")` from the R console).

A template workflow for producing national/regional dashboards is available at https://github.com/southwick-associates/dashboard-template
