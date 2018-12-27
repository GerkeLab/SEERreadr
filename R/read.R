#' Read SEER Columns from SAS file
#' 
#' Reads the SEER column specification from the SAS import file.
#' 
#' @param file Path to `read.seer.research.sas` file.
#' @param use_col_desc Should the column description be used for the column
#'   names? If `FALSE` (default), column descriptions are added as variable
#'   labels using [labelled::var_label()].
#' @examples 
#' seer_col_pos <- seer_read_col_positions()
#' head(seer_col_pos)
#' 
#' @export
seer_read_col_positions <- function(
  file = "https://seer.cancer.gov/manuals/read.seer.research.nov2017.sas",
  raw = FALSE,
  use_col_desc = FALSE
) {
  rx_begin    <- "@ [[:digit:]]{1,3}"
  rx_width    <- "\\$char[[:digit:]]{1,2}"
  rx_col_name <- "[[:upper:]]+[[:upper:][:digit:][:punct:]]+"
  rx_col_desc <- "\\/\\*.+\\*\\/"
  
  ret <- 
    readr::read_lines(file) %>% 
    dplyr::data_frame(raw = .) %>% 
    # remove first few rows by insisting an @ that defines 
    # the start index of that field
    dplyr::filter(stringr::str_detect(raw, "@")) %>% 
    # extract out the begin, width and column name+description fields
    mutate(
      begin    = stringr::str_extract(raw, rx_begin),
      begin    = stringr::str_remove(begin, "@\\s*"),
      width    = stringr::str_extract(raw, rx_width),
      width    = stringr::str_remove(width, "\\$char"),
      col_name = stringr::str_extract(raw, rx_col_name),
      col_desc = stringr::str_extract(raw, rx_col_desc),
      col_desc = stringr::str_remove_all(col_desc, "[/*]{2}"),
      col_desc = stringr::str_trim(col_desc)
    ) %>% 
    ## coerce to integers
    dplyr::mutate_at(dplyr::vars(begin, width), as.integer) %>% 
    ## calculate the end position
    mutate(begin = begin - 1, end = begin + width) %>% 
    select(col_name, col_desc, begin, end, width)
  
  select(ret, begin, end,
         col_names = if (use_col_desc) "col_desc" else "col_name", 
         col_labels = if (use_col_desc) "col_name" else "col_desc")
}

#' Read SEER Fixed Width File
#' 
#' Reads a SEER fixed width file using the column dictionary in the provided SAS
#' file (see [seer_read_col_positions()] for more details). Note that the results
#' are the raw data reported by SEER with no transformations -- all fields are
#' imported as character strings by default. You can change this by specifying
#' the `col_types` argument using [readr] column specification via
#' [readr::cols()], or you can set `col_types = NULL` to let [readr] guess the
#' column type.
#'
#' @param file Path to SEER fixed width file.
#' @param col_positions SEER column positions, see [seer_read_col_positions()].
#' @param col_types Specification for column types, default is to return all
#'   as character strings. Use `NULL` to rely on [readr] or see 
#'   [readr::read_fwf()] for further details.
#' @inheritDotParams readr::read_fwf locale na comment skip n_max guess_max progress
#' @export
seer_read_fwf <- function(
  file,
  col_positions = seer_read_col_positions(),
  ...,
  col_types = readr::cols(.default = readr::col_character())
) {
  # Check that dictionary has correct columns
  expected_cols <- c("begin", "end", "col_names")
  missing_cols <- setdiff(expected_cols, names(col_positions))
  if (length(missing_cols)) {
    stop(paste(missing_cols, collapse = ", "), 
         " required but not found in `col_positions`. ",
         "Please use or refer to `seer_read_col_positions()` ",
         "or `readr::fwf_positions()`.")
  }
  col_labels <- if ("col_labels" %in% names(col_positions)) col_positions$col_labels
  col_positions <- col_positions[, expected_cols]
  
  if (is.null(col_types)) col_types <- readr::cols()
  
  ret <- 
    readr::read_fwf(
      file = file, 
      col_positions = col_positions,
      col_types = col_types,
      ...
    )
  if (!is.null(col_labels)) {
    col_labels <- as.list(col_labels)
    names(col_labels) <- col_positions$col_names
    labelled::var_label(ret) <- col_labels
  }
  ret
}