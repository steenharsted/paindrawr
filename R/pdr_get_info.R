#' Extract information from pain drawing objects
#'
#' Extracts metadata or variables from a pain drawing (`data_col`) list-column.
#' Simple elements (such as `".id"` or `".timestamp"`) are returned as a vector,
#' whereas columns within the nested `.strokes` or `.points` tibbles are
#' returned as a list with one element per pain drawing.
#'
#' @param data_col A valid pain drawing object (typically a list-column) as checked
#'   by [pdr_check_data()].
#' @param var A character vector of length 1 or 2 specifying what information
#'   to extract.
#'
#'   If `var` has length 1, the value should be the name of an element in each
#'   pain drawing (e.g. `".id"`).
#'
#'   If `var` has length 2, the first element must be either `".strokes"` or
#'   `".points"`, and the second element must be the name of a column in the
#'   corresponding tibble (e.g. `c(".strokes", ".alpha")`).
#'
#' @return
#' If `var[1]` refers to a regular element, a vector of length `length(data_col)` is
#' returned.
#'
#' If `var[1]` is `".strokes"` or `".points"`, a list of length `length(data_col)`
#' is returned, where each element contains the selected column from the
#' corresponding tibble.
#'
#' @examples
#' # Extract ids
#' pdr_example_data |> pdr_get_info(pdr_data, ".id")
#'
#' # Extract x coordinates from the .points tibble
#' pdr_example_data |> pdr_get_info(pdr_data, ".strokes", ".alpha")
#'#'
#' @seealso [pdr_check_data()]
#'
#' @export

pdr_get_info <- function(paindrawr_data, id1=".id", id2=NULL) {
  # paindrawr_data is expected to be a valid data_col list-col
  # var1 should be an element name in data_col (list)
  # var2 should be an element in var1, if var1 is tibble/list

  if(is.null(id1)) {
    warning("No id1 parameter supplied")
    return(NA)
  }

  if(is.null(id2)) {
    if(id1 %in% c(".points", ".strokes", ".polygons")) {
      paindrawr_data |> 
        purrr::map(id1)
    } else {
      paindrawr_data |> 
        purrr::map(id1) |>    # Just the one element
        purrr::list_simplify() # Convert to int, chr, num, ..
        
    }
  } else {
    paindrawr_data |>
      purrr::map(id1) |>
      purrr::map(~dplyr::pull(.x, id2)) |>
      purrr::list_simplify()
  }
}