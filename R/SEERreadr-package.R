#' @importFrom dplyr mutate select left_join
#' @keywords internal
"_PACKAGE"

#' ICD-O-3 SEER Site/Histology Validation List
#' 
#' The list of ICD-O-3 SEER Site/Histology codes.
#' 
#' @format A data frame with 50644 rows and 10 variables
#' \describe{
#'   \item{site}{}
#'   \item{site_desc}{}
#'   \item{histology}{}
#'   \item{histology_desc}{}
#'   \item{histology_behavior}{}
#'   \item{histology_behavior_desc}{}
#' }
#' @source <https://seer.cancer.gov/icd-o-3/>
"icdo3"

#' SEER Data Dictionary
#'
#' The SEER Data dictionary is built (semi-manually) from the SEER Research Data
#' Record Description data dictionary for 1973-2015 SEER incidence data files
#' available at <https://seer.cancer.gov/data-software/documentation/ascii-files.html>.
#' The current version used in this package is the April 2018 documentation version.
#' 
#' @format A named list of tibbles containing reference codes and their corresponding
#'   labels, as used in the SEER data.
#' @source <https://seer.cancer.gov/data-software/documentation/seerstat/nov2017/TextData.FileDescription.pdf>
#' @name seer_data_dictionary
#' @export
"seer_data_dictionary"