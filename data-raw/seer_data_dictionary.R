library(tidyverse)
library(pdftools) # see https://github.com/ropensci/pdftools


# Functions ---------------------------------------------------------------
download_file_to_data_raw <- function(url, where = "data-raw") {
  url <- url
  file <- here::here("data-raw", basename(url))
  if (!file.exists(file)) {
    download.file(url, file)
  }
  file
}

to_titlecase <- function(x) tools::toTitleCase(tolower(x))

str_split_lines <- function(x) str_split(x, "\n")[[1]]

str_find_code <- function(x, offset = 0) {
  x[(str_which(x, "^\\s*Code") + offset):length(x)]
}

str_starts_with_num <- function(x) str_subset(x, "^\\s*\\d")

str_match_code_desc <- function(x) {
  str_match(x, "([0-9.-]+)\\s+(.+)$") %>% 
    .[, 2:3] %>% 
    as_data_frame() %>% 
    rename("Code" = V1, "Description" = V2)
}

get_item_number <- function(x, number = 0, rx_item = "^\\s*NAACCR Item") {
  ids <- str_which(x, rx_item)
  if (!number) {
    message("There are ", length(ids), " items on this page.")
    return(invisible(ids))
  }
  stopifnot(number <= length(ids))
  end <- if (number == length(ids)) length(x) + 1 else ids[number + 1]
  x[ids[number]:(end - 1)]
}

str_simple_extract <- function(x, split = TRUE) {
  if (split) x <- str_split_lines(x)
  x %>% 
    str_find_code() %>% 
    str_trim() %>% 
    str_starts_with_num() %>% 
    str_match_code_desc()
}

expand_range <- function(x, col = Code, ...) {
  col <- rlang::enquo(col)
  col_name <- rlang::quo_name(col)
  x %>% 
    mutate(.id = row_number()) %>% 
    nest(!!col) %>% 
    mutate(
      data = map(data, col_name),
      data = map(data, expand_range_, ...)
    ) %>% 
    unnest() %>% 
    rename(!!col_name := data) %>% 
    select(colnames(x))
}

expand_range_ <- function(x, rx_range = "\\s*-\\s*", by = 1L, format = "%03d") {
  if (!str_detect(x, rx_range)) return(x)
  x <- str_split(x, rx_range)[[1]]
  stopifnot(length(x) == 2)
  
  sprintf(format, seq(as.numeric(x[1]), as.numeric(x[2]), by))
}

mark_usage <- function(x, direct = TRUE, lookupable = TRUE) {
  if (direct) attributes(x)$direct <- TRUE
  if (lookupable) attributes(x)$lookupable <- TRUE
  x
}

# Get Text from PDF file --------------------------------------------------
pdf_url <- "https://seer.cancer.gov/data-software/documentation/seerstat/nov2017/TextData.FileDescription.pdf"
pdf_file <- download_file_to_data_raw(pdf_url)

txt <- pdf_text(pdf_file)

seer_data_dictionary <- list()

# Registry ID -------------------------------------------------------------
## Field Description: A unique code assigned to each participating SEER
## registry. The number identifies the registry sending the record and what
## population the data are based on.

seer_data_dictionary$REG <- 
  txt[10] %>% 
  str_split_lines() %>% 
  str_subset("000000") %>% 
  str_trim() %>% 
  str_remove("\\*+") %>% 
  str_match_code_desc() %>% 
  mark_usage() 
  

# Marital Status at DX ----------------------------------------------------
## Field Description: This data item identifies the patient’s marital status at
## the time of diagnosis for the reportable tumor.

seer_data_dictionary$MAR_STAT <- txt[11] %>% str_simple_extract() %>% 
  mark_usage() 
  


# Race/Ethnicity ----------------------------------------------------------
## Field Description: Recode which gives priority to non-white races for
## persons of mixed races. Note that not all codes were in effect for all years.

seer_data_dictionary$RACE1V <- txt[12] %>% str_simple_extract() %>% 
  mark_usage() 


# Derived Hispanic Origin -------------------------------------------------
## Field Description: The NAACCR Hispanic Identification Algorithm (NHIA) is a
## computerized  algorithm that uses a combination of variables to directly or
## indirectlyclassify cases as Hispanic for analytic purposes.

seer_data_dictionary$NHIADE <- 
  txt[13] %>%
  str_split_lines() %>% 
  get_item_number(1) %>%
  str_simple_extract(split = FALSE) %>% 
  mark_usage() 


# Sex ---------------------------------------------------------------------
## Field Description: This data item identifies the sex of the patient at diagnosis.

seer_data_dictionary$SEX <- 
  txt[13] %>% 
  str_split_lines() %>% 
  get_item_number(2) %>% 
  str_simple_extract(split = FALSE) %>% 
  mark_usage() 
  


# Age at Diagnosis --------------------------------------------------------
## Field Description: This data item represents the age of the patient at
## diagnosis for this cancer. The code is three digits and represents the
## patient’s actual age in years.

seer_data_dictionary$AGE_DX <- 
  txt[13] %>% 
  str_split_lines() %>% 
  get_item_number(3) %>% 
  str_simple_extract(split = FALSE) %>% 
  expand_range() %>% 
  mutate(
    Description = ifelse(Code != "999", 
                         paste(as.integer(Code)), 
                         NA_character_)
  ) %>% 
  mark_usage() 
  


# ICD-O-3 -----------------------------------------------------------------
seer_icdo3_file <- download_file_to_data_raw(
  "https://seer.cancer.gov/icd-o-3/sitetype.icdo3.20180323.xls"
)

icdo3 <- readxl::read_xls(seer_icdo3_file) %>% 
  mutate(`Site recode` = str_remove_all(`Site recode`, "C")) %>% 
  separate_rows(`Site recode`, sep = ",") %>% 
  expand_range(`Site recode`) %>% 
  mutate(`Site recode` = paste0("C", `Site recode`)) %>% 
  rename(
    site = `Site recode`,
    site_desc = `Site Description`,
    histology = Histology,
    histology_desc = `Histology Description`,
    histology_behavior = `Histology/Behavior`,
    histology_behavior_desc = `Histology/Behavior Description`
  ) %>% 
  mutate_at(vars(ends_with("_desc")), to_titlecase)

usethis::use_data(icdo3, overwrite = TRUE)

# Primary Site ------------------------------------------------------------
## Field Description: This data item identifies the site in which the primary
## tumor originated. See the International Classification of Diseases for
## Oncology, Third Edition (ICD-O-3) for topography codes. The decimal point is
## eliminated. Cases diagnosed 1977-1991 were coded using the International
## Classification of Diseases for Oncology, 1976 Edition (ICD-O-1976). Prior to
## 1977 diagnoses, cases were coded using the Manual of Tumor Nomenclature and
## Coding, 1968 (MOTNAC). All cases 1973-1991 were machine-converted to ICD-O-2
## codes without complete hand review.

seer_data_dictionary$PRIMSITE <-
  icdo3 %>% 
  select(1:2) %>% 
  mark_usage() 

# Grade -------------------------------------------------------------------
## Field Description: Grading and differentiation codes of 1-4, 9 are defined in
## ICD-O-2; 1992. Grade information may be incomplete for cases diagnosed before
## 1977. In the early 1980’s, additional codes specifying T-cell, B-cell, or null
## cell involvement in lymphomas and leukemias (histologies M9590-9940) were
## introduced by SEER. Because the reporting requirements and medical terminology
## have changed over time, care should be exercised when analyzing this
## information.

seer_data_dictionary$GRADE <- 
  txt[21] %>% 
  str_split_lines() %>% 
  get_item_number(1) %>%
  str_simple_extract(split = FALSE) %>% 
  mark_usage() 


# Collaborative Stage Site-Specific Factors -------------------------------
## Starting with the 1973-2011 SEER Research Data (November 2013 submission),
## additional Collaborative Stage Site-Specific Factor (CS SSF) fields are
## available in the standard research data for analysis in the rate, frequency
## and case listing sessions. Previously, they were only available through a
## custom database.

seer_csssf_file <- download_file_to_data_raw(
  "https://seer.cancer.gov/seerstat/databases/ssf/SSFs.Released.v205.Nov2016.xls"
)

seer_data_dictionary$CSSSF <-
  readxl::read_xls(seer_csssf_file, sheet = "Alphabetical", skip = 3) %>% 
  gather(variable, label, -`Schema Name`) %>% 
  mutate(
    variable = str_remove(variable, "SSF\\s*"),
    variable = paste0("CS", variable, "SITE")
  ) %>% 
  filter(!is.na(label))


# SEER Type of Follow-Up --------------------------------------------------
## Field Description: This item codes the type of follow-up expected for a SEER case.

seer_data_dictionary$TYPE_FU <- 
  txt[45] %>% 
  str_split_lines() %>% 
  get_item_number(3) %>% 
  str_find_code(1) %>% 
  str_subset("^\\s{4}") %>% 
  str_match("\\s{7}(.)(\\s+(.+))?$") %>% 
  .[, c(2, 4)] %>% 
  as_data_frame() %>% 
  mutate(V1 = str_replace(V1, " ", "4")) %>% 
  group_by(V1) %>% 
  summarize(V2 = paste(V2[!is.na(V2)], collapse = " ")) %>% 
  rename(Code = V1, Description = V2) %>% 
  mark_usage() 


# Age Recode --------------------------------------------------------------
## Field Description: The age recode variable is based on Age at Diagnosis
## (single-year ages). The groupings used in the age recode variable are
## determined by the age groupings in the population data. This recode has 19 age
## groups in the age recode variable (< 1 year, 1-4 years, 5-9 years, ..., 85+
## years).

seer_data_dictionary$AGE_1REC <-
  txt[46] %>% 
  str_split_lines() %>% 
  get_item_number(1) %>%
  str_simple_extract(FALSE) %>% 
  mutate(
    Description = str_remove(Description, "Ages? "),
    Description = str_replace_all(Description, "0(\\d)", "\\1"),
    Description = if_else(Code == "99", NA_character_, Description)
  ) %>% 
  mark_usage() 


# Race Recode (White, Black, Other) ---------------------------------------
## Field Description: Race recode is based on the race variables and the American
## Indian/Native American IHS link variable. This recode should be used to link
## to the populations for white, black and other. It is independent of Hispanic
## ethnicity. For more information, see
## <http://seer.cancer.gov/seerstat/variables/seer/race_ethnicity>.

seer_data_dictionary$RAC_RECA <-
  txt[56] %>% 
  str_split_lines() %>% 
  get_item_number(1) %>%
  .[!str_detect(., "April")] %>% 
  str_simple_extract(FALSE) %>% 
  mark_usage() 


# Vital Status Recode -----------------------------------------------------
## Field Description: Any patient that dies after the follow-up cut-off date is
## recoded to alive as of the cut-off date.

seer_data_dictionary$STAT_REC <- 
  txt[61] %>% 
  str_split_lines() %>% 
  get_item_number(3) %>% 
  str_simple_extract(FALSE) %>% 
  mark_usage() 
  

# CS Schema v0204+ --------------------------------------------------------
## Field Description: CS information is collected under the specifications of a
## particular schema based on site and histology. This recode should be used in
## any analysis of AJCC 7th ed stage and T, N, M.

# NOTE: This table links CS Site Specific Factors to the CS_SITE columns

seer_data_dictionary$CS0204SCHEMA <- 
  txt[52:56] %>% 
  paste(collapse = "\n") %>%
  str_split_lines() %>% 
  get_item_number(1) %>% 
  .[!str_detect(., "April")] %>% 
  .[-str_which(., "Code\\s+Description")[-1]] %>% 
  str_simple_extract(FALSE) %>% 
  mark_usage() 
  

# SEER Cause-Specific Death Classification --------------------------------
## Field Description: Created for use in cause-specific survival. This variable
## designates that the person died of their cancer for cause-specific survival.
## For more information, see <http://seer.cancer.gov/causespecific>.

seer_data_dictionary$VSRTSADX <- 
  txt[63] %>% 
  str_split_lines() %>% 
  get_item_number(2) %>% 
  str_simple_extract(FALSE) %>% 
  mutate(Description = na_if(Description, "N/A not first tumor")) %>% 
  mark_usage()

# SEER Other Cause of Death Classification --------------------------------
## Field Description: Created for use in left-truncated life table session. This
## variable designates that the person died of causes other than their cancer.
## For more information, see <http://seer.cancer.gov/causespecific>.

seer_data_dictionary$ODTHCLASS <- 
  txt[63] %>% 
  str_split_lines() %>% 
  get_item_number(3) %>%
  str_simple_extract(FALSE) %>% 
  mutate(Description = na_if(Description, "N/A not first tumor")) %>% 
  mark_usage()



# Save SEER data dictionary -----------------------------------------------
usethis::use_data(seer_data_dictionary, internal = TRUE, overwrite = TRUE)
