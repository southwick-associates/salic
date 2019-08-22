
# salic: An R Package to Prepare Agency Dashboard Data

This R package includes a set of functions for summarizing agency data for dashboards, developed under the leadership of the Association of Fish and Wildlife Agencies.

## Installation

You'll need a version of R installed (version 3.5.0 or greater). Binary installers are included (here)

## Usage

I recommend starting with [Introduction to salic](/vignettes/README.md) (also available by running `vignette("salic")` from the R console).

## Background (temp)

Package salic lays out a workflow for use in AFWA's national/regional dashboard project. It's intended to simplify/standardize the production of the necessary summary data by providing a set of easy-to-use functions. This vignette illustrates the workflow using sample data (included as part of the salic installation).

Processing data for the dashboard can be divided into 3 parts:

1. **Standardized License Data**: Getting license data into a standard (anonymized) format well-suited for generalized data processing functions.

2. **License History**: Converting into a license history format for easy calculation of dashboard metrics.

3. **Dashboard Metrics**: Applying estimation functions to the license history data, which produces the summaries used as input to the dashboard software.

## 1. Standardized License Data

It's much easier to build a generalized set of tools if we know what the input data looks like. For this reason, salic is pretty strict about how the input license data should be formatted. The process necessary to produce this format will vary by state, but fortunately the data needs for producing dashboards are fairly minimal (and salic provides the `data_check()` function to facilitate this). 

### Customer & License IDs

The fundamental assumption of salic is that customers can be identified across transactions using a customer ID (and similarly, license types can be identified using a license ID). 

## News

See [NEWS.md](./NEWS.md) for version-specific release notes.