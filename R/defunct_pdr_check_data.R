#' Validate a pain drawing data structure
#'
#' A pain drawing data structure must comply with certain criteria -- see Details below. 
#' This function returns TRUE or FALSE and optionally emits diagnostic messages.
#'
#' #' @details
#' A valid pain drawing data structure must satisfy all of the following:
#'
#' **Top-level structure**
#' * An unnamed list with one element per pain drawing
#' 
#' **Second-level structure**
#' * A list with one named element per variable
#'     - Must contain elements: `.id`, `.file`, `.version`, `.units`, `.width`, `.height`, `.timestamp`, `.app`, `.strokes`, `.points`
#'
#' **Second-level element types**
#' * `.id`: character (unique identifier)
#' * `.version`, `.width`, `.height`: integer
#' * `.file`, `.units`, `.timestamp`, `.app`: character or NA
#' * `.strokes`, `.points`: tibble or NA
#'
#' **Element constraints**
#' * `.id` values must be unique
#'
#' **`.strokes` tibble or NA (stroke metadata)**
#' If .strokes is a tibble, it must be structured as:
#' * Columns: `.index`, `.draw_input_type`, `.tool`, `.tool_width`, `.color`, `.spray_radius`, `.point_density`, `.alpha`, `scaleFactor`, `dpr`
#' * Types: integer, character, integer, character, integer, character, integer, integer, numeric, numeric, integer
#' * One row per stroke (`.index` must be unique)
#' * Exactly one unique value of `.color` per tibble
#'
#' **`.points` tibble or NA (stroke coordinates)**
#' If .points is a tibble, it must be structured as:
#' * Columns: `.index`, `.x`, `.y`
#' * All integer
#' * `.index` values correspond to those in the matching `.strokes` tibble
#' 
#' #' **Relationship between `.strokes` and `.points`**
#' * The `.strokes` tibble defines strokes (one row per stroke)
#' * The `.points` tibble defines coordinates (multiple rows per stroke)
#' * The `.index` column links them:
#'   - `.strokes$.index`: unique stroke identifiers
#'   - `.points$.index`: repeats to associate coordinates with a stroke
#' * If one is NA, the other must also be NA
#' 
#' 
#' @param d An object to validate.
#'
#' @returns
#' `TRUE` if `d` is valid, otherwise `FALSE`.
#' If `verbose = TRUE`, diagnostic messages are emitted.
#'
#' @export
#' @examples
#' pdr_example_data |> pdr_check_data(pdr_data, verbose = FALSE) # Will return TRUE
#'
#' pdr_check_data(letters[1:10]) # Will return FALSE and provide details
#'
pdr_check_data <- function(.data, paindrawr_data = pdr_data, verbose = TRUE) {
  d <- .data |> dplyr::pull({{paindrawr_data}})

  if (!is.list(d)) {
    warning("Data 'd' is not a list", call. = FALSE)
    return(FALSE)
  }

  # ---- helpers ----
  fail <- function(msg) {
    warning(msg, call. = FALSE)
    return(FALSE)
  }

  ok <- function(msg) {
    if (verbose) message(msg)
  }

  is_na_scalar <- function(x) {
    identical(x, NA)
  }

  is_list_or_na <- function(x) {
    is.list(x) || is_na_scalar(x)
  }

  has_names <- function(x, required) {
    all(required %in% names(x))
  }

  # ---- validators for nested tibbles ----
  valid_strokes_tbl <- function(x) {
    tibble::is_tibble(x) &&
      setequal(names(x), c(".index", ".draw_input_type", ".tool", ".tool_width", ".color", ".alpha", ".point_density", ".spray_radius", "scaleFactor", "dpr")) &&
      is.integer(x$.index) &&
      is.character(x$.draw_input_type) &&
      is.character(x$.tool) &&
      is.integer(x$.tool_width) &&
      is.character(x$.color) &&
      is.numeric(x$.alpha) &&
      is.integer(x$.point_density) &&
      is.integer(x$.spray_radius) &&
      is.numeric(x$scaleFactor) &&
      is.integer(x$dpr) &&
      length(unique(x$.color)) == 1 &&
      !any(duplicated(x$.index))
  }

  valid_points_tbl <- function(x) {
    tibble::is_tibble(x) &&
      setequal(names(x), c(".index", ".x", ".y")) &&
      is.integer(x$.index) &&
      is.integer(x$.x) &&
      is.integer(x$.y)
  }

int_checker <- function(d) {
  # This function checks each element of the pain drawing
  # data structure (which is a list) -- each element should
  # also be a list (of named elements)

  if(verbose) {
    try({    
      message("") # Empty line
      message(paste0("Element with ID ", d$.id, ":"))
    })
  }

  # ---- top-level list ----
  if (!is.list(d)) {
    return(fail("Data is a list: FAIL -- NOTE data should not simply be a list of elements '.id', '.units', '.width', '.height', '.timestamp', '.strokes', '.points', but a list of a list of such elements."))
  }
  ok("Data is a list: OK")

  # ---- required columns ----
  required_cols <- c(".id", ".file", ".version", ".units", ".width", ".height", ".timestamp", ".app", ".strokes", ".points")
  if (!has_names(d, required_cols)) {
    return(fail("Missing required elements"))
  }
  ok("Required elements present: OK")

  # ---- column types ----
  if (!(is.character(d$.id) &&
        is.character(d$.file) &&
        is.character(d$.version) &&
        is.integer(d$.width) &&
        is.integer(d$.height) &&
        is.character(d$.units) &&
        is.character(d$.timestamp) &&
        is.character(d$.app) &&
        (tibble::is_tibble(d$.strokes) || is.na(d$.strokes)) &&
        (tibble::is_tibble(d$.points) || is.na(d$.points)))) {
    warning(str(d))
    return(fail("Element types incorrect"))
  }
  ok("Element types: OK")

  # ---- unique IDs ----
  if (any(duplicated(d$.id))) {
    return(fail("IDs must be unique"))
  }
  ok("IDs unique: OK")

  # ---- list column '.strokes' (allow NA) ----
  if (!is_na_scalar(d$.strokes) && !valid_strokes_tbl(d$.strokes)) {
    return(fail("Invalid column in '.strokes'"))
  }
  ok("'.strokes' column valid: OK")

  # ---- list column '.points' (allow NA) ----
  if (!is_na_scalar(d$.points) && !valid_points_tbl(d$.points)) {
    return(fail("Invalid column in '.points'"))
  }
  ok("'.points' column valid: OK")

  # ---- if .points is NA so must .strokes and vice versa ----
  if(is_na_scalar(d$.points) && !is_na_scalar(d$.points)) {
    return(fail(".points is NA, but .strokes is not"))
  }
  if(is_na_scalar(d$.points) && !is_na_scalar(d$.points)) {
    return(fail(".strokes is NA, but .points is not"))
  }

  # ---- identical '.index' in list column '.points' and '.strokes' 
  if(!is_na_scalar(d$.points) && !is_na_scalar(d$.points)) {
    if (!setequal(unique(d$.points$.index), d$.strokes$.index)) {
      return(fail("Discrepancies in column '.index' in columns '.points' and '.strokes'"))
    }
  }
  ok("Column '.index' in list columns '.poins' and '.strokes' have same values")

  ok("Data structure is valid pain drawing data structure: OK")
  TRUE
  }
  # ---- end helpers ----

  purrr::every(d, int_checker)
}

