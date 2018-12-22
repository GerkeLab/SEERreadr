#' Recode SEER Variables
#' 
#' Recodes SEER variables according to the data dictionary.
#' Only recodes variables that can be directly looked up.
#' 
#' @param .data The SEER data set
#' @export
recode_seer <- function(.data) {
  automatable <- purrr::map_lgl(seer_data_dictionary, is_directly_replaceable)
  for (column in intersect(names(.data), names(automatable))) {
    if (!is_lookupable(seer_data_dictionary[[column]])) {
      join_by <- setNames(column, "Code")
      .data <- left_join(.data, seer_data_dictionary[[column]], by = join_by)
    }
    lookup <- as_lookup_table(seer_data_dictionary[[column]])
    .data[[column]] <- lookup[.data[[column]]]
    .data[[column]] <- factor(.data[[column]])
  }
  .data
}

#' Rename Site-Specific Variables
#' 
#' There are a number of variables that cover site-specific factors.
#' This function renames those variables according the their labels.
#' The resulting names are long but descriptive.
#' 
#' @param .data The SEER data set
#' @param name_formatter A function that takes will be applied to the
#'   site-specific factor descriptions to convert into valid variable
#'   names. See [snakecase::to_snake_case()].
#' @export
rename_site_specific <- function(.data, 
                                 name_formatter = snakecase::to_snake_case) {
  stopifnot("CS0204SCHEMA" %in% names(.data))
  
  # Check if the schema has been recoded
  schemas <- seer_data_dictionary$CSSSF
  match_on <- c("Schema Name" = "CS0204SCHEMA")
  .data$CS0204SCHEMA <- as.character(.data$CS0204SCHEMA)
  if (max(nchar(.data$CS0204SCHEMA[1:min(nrow(.data), 1000)])) < 4) {
    names(match_on) <- "Code"
    schemas <- left_join(schemas, seer_data_dictionary$CS0204SCHEMA,
                         by = c("Schema Name" = "Description"))
  }
  
  schemas <- schemas %>% 
    dplyr::semi_join(.data, by = match_on) %>% 
    mutate(label = snakecase::to_snake_case(label)) %>% 
    select(union(names(match_on), c("variable", "label"))) %>% 
    tidyr::nest(-!!names(match_on), .key = "var") %>% 
    mutate(!!names(match_on) := as.character(!!rlang::sym(names(match_on))))
  
  
  .data %>% 
    tidyr::nest(-CS0204SCHEMA) %>% 
    left_join(schemas, by = c("CS0204SCHEMA" = names(match_on))) %>% 
    mutate(
      var = purrr::map(var, as_lookup_table),
      data = purrr::map2(data, var, ~ rename_columns(.x, .y))
    ) %>% 
    select(-var) %>% 
    tidyr::unnest()
}

rename_columns <- function(x, new_names) {
  if (is.null(new_names) || !length(new_names)) return(x)
  new_names <- new_names[intersect(colnames(x), names(new_names))]
  colnames(x)[which(colnames(x) %in% names(new_names))] <- new_names
  x
}

as_lookup_table <- function(x, name = 1, value = 2) {
  if (is.null(x)) return(NULL)
  setNames(x[[2]], paste(x[[1]]))
}

is_directly_replaceable <- function(x) isTRUE(attr(x, "direct"))
is_lookupable <- function(x) isTRUE(attr(x, "lookupable"))