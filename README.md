
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SEERreadr

<!-- badges: start -->

<!-- badges: end -->

A small package for reading SEER fixed width files.

<!-- README start -->

## Installation

**SEERreadr** can be installed from GitHub with

``` r
# install.packages("remotes")
remotes::install_github("gerkelab/SEERreadr", upgrade = FALSE)
```

## What does this package do?

### Read SEER Data Files

The main workhorse of this package is `seer_read_fwf()`. This function
wraps `readr::read_fwf()` to import the SEER fixed-width ASCII data
files, using the column names and field width definitions in the [SEER
SAS
script](https://seer.cancer.gov/manuals/read.seer.research.nov2017.sas).

The data files are available from the [SEER Data & Software
page](https://seer.cancer.gov/data-software/), where users must request
access prior to downloading. The SAS script is included in the file
download, or avilable online. The online version is used by
`seer_read_fwf()`, but a local version can be specified in the helper
function `seer_read_col_positions("local_file.sas")`.

``` r
library(SEERreadr)
x <- seer_read_fwf("incidence/yr1973_2015.seer9/MALEGEN.TXT")
```

### Recode SEER Variables

Two additional functions are provided to help recode the SEER data.
`seer_recode()` uses the `seer_data_dictionary` data provided in this
package to automatically recode all variables with a one-to-one
correspondence, for example:

``` r
seer_data_dictionary$SEX
#> # A tibble: 2 x 2
#>   Code  Description
#> * <chr> <chr>      
#> 1 1     Male       
#> 2 2     Female
```

The package also includes the function `seer_rename_site_specific()`
that can be used to replace the site-specific variables with their
corresponding labels, formatted appropriately to serve as variable
names. As an example, CSSSF variables for Breast cancer would be renamed
according to the following
table.

| Original Variable | New Variable Name                                                     |
| :---------------- | :-------------------------------------------------------------------- |
| `CS1SITE`         | `estrogen_receptor_er_assay_2004`                                     |
| `CS2SITE`         | `progesterone_receptor_pr_assay_2004`                                 |
| `CS3SITE`         | `number_of_positive_ipsilateral_level_i_ii_axillary_lymph_nodes_2004` |
| `CS4SITE`         | `immunohistochemistry_ihc_of_regional_lymph_nodes_2004`               |
| `CS5SITE`         | `molecular_mol_studies_of_regional_lymph_nodes_2004`                  |
| `CS6SITE`         | `size_of_tumor_invasive_component_2004`                               |
| `CS7SITE`         | `nottingham_or_bloom_richardson_br_score_grade_2010`                  |
| `CS15SITE`        | `her_2_summary_result_of_testing_2010`                                |

## Thanks

Thank you to [Vincent Major](https://github.com/vincentmajor) for making
available the scripts in
[SEER\_read\_fwf](https://github.com/vincentmajor/SEER_read_fwf), which
provided a foundation for this package.
