map_valid_lovs <- function(values, lkup_vector) {
  names(lkup_vector) <- tolower(names(lkup_vector))
  # Deal with multiple values
  values <- stringr::str_split(values, pattern = ';', simplify = FALSE)
  values <- unlist(values)
  values <- values[values != ""]
  values <- stringr::str_trim(values, side = 'both')
  out <- purrr::map_chr(values, function(x) unname(lkup_vector[tolower(x)]))

  out[is.na(out)] <- values[is.na(out)] # replace non-mapped values, by original values
  out <- unique(stringr::str_trim(out))

  purrr::map(out, function(x) assertthat::assert_that(are_valid_lovs(x, lkup_vector),
                                                      msg = paste0("No valid mapping found in lkup_vector for: ", x)))

  out <- paste(out, collapse = ';')

  return(out)
}

are_valid_lovs <- function(values, accepted_values) {
  all(values %in% accepted_values)
}

safe_unbox <- purrr::possibly(jsonlite::unbox, otherwise = '')
safe_assign <- function(x) {if (length(x) > 0) {x} else {""}}

safe_assert <- function(file_value, orig_value) {
  assertthat::assert_that((is.null(file_value) && is.null(orig_value)) || (file_value == gsub("[\n]", "", orig_value)))
}
