
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wmo

<!-- badges: start -->

<!-- badges: end -->

The goal of `wmo` is to collect personal utility functions, data sets
used for recurrent analyses, and document templates.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wmoldham/wmo")
renv::install("wmoldham/wmo)
```

## Functions

#### remove\_outliers

Function to remove outliers using the median absolute deviation
approach.

#### theme\_wmo

Personalized `ggplot2` theme.

## Data

#### correction\_factors

Experimental peak areas are multiplied by these correction factors to
account for quadrupole bias in selected ion monitoring mode. These data
are from the 2019-04-10 experiment.

## Templates

#### data-analysis

Personalized `rmarkdown` template for routine data analysis optimized
for PDF output.

#### draft-styles

These are the foundation to start optimizing a Word template for
`rmarkdown` output. Open `draft-styles.docx`, then change the styles
corresponding to various text types and save as a new template document.
