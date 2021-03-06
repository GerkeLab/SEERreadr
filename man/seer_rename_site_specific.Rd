% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{seer_rename_site_specific}
\alias{seer_rename_site_specific}
\title{Rename SEER Site-Specific Variables}
\usage{
seer_rename_site_specific(.data,
  name_formatter = snakecase::to_snake_case)
}
\arguments{
\item{.data}{The SEER data set, from \code{\link[=read_seer_fwf]{read_seer_fwf()}} with
\code{use_col_desc = FALSE}.}

\item{name_formatter}{A function that takes will be applied to the
site-specific factor descriptions to convert into valid variable names. See
\code{\link[snakecase:to_snake_case]{snakecase::to_snake_case()}}.}
}
\description{
There are a number of variables that cover site-specific factors. This
function renames those variables according the their labels. The resulting
names are long but descriptive.
}
