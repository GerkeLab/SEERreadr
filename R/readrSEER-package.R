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