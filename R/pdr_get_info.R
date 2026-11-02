#' Extract information from pain drawing objects
#'
#' Extracts metadata or variables from a pdr_list (see [pdr_match()]), e.g.
#' the list-column `pdr_data`. Simple elements (such as `".id"` or
#' `".timestamp"`) are returned as a vector, whereas the nested elements
#' `.points`, `.strokes`, `.polygons`, `.rgba` and `.stencil` are returned as
#' a list with one element per pdr_drawing.
#'
#' @param pdr_list A pdr_list (see [pdr_match()]), e.g. the `pdr_data`
#'   column of a pdr_tibble.
#' @param id1 A string specifying which element of each pdr_drawing to get.
#'   Default `".id"`.
#' @param id2 Optional string naming a column within `id1` (when `id1` is a
#'   tibble such as `.points` or `.strokes`) to extract.
#'
#' @returns
#' If `id2` is `NULL` and `id1` is one of `.points`, `.strokes`, `.polygons`,
#' `.rgba` or `.stencil`: a list of the same length as `pdr_list`, with the
#' element from each drawing. If `id2` is `NULL` and `id1` is any other
#' element: a vector of the same length as `pdr_list`.
#' If `id2` is given: the column `id2` of `id1` for each drawing, as a list
#' with one element per drawing (simplified to a vector only if every drawing
#' contributes exactly one value).
#'
#' @examples
#' # Extract ids (a vector)
#' pdr_example_data |>
#'   dplyr::mutate(id = pdr_get_info(pdr_data, ".id"))
#'
#' # Extract the .x coordinates from the .points tibbles (a list)
#' pdr_example_data |>
#'   dplyr::mutate(x_coord = pdr_get_info(pdr_data, ".points", ".x"))
#'
#' @seealso [pdr_match()]
#'
#' @export

pdr_get_info <- function(pdr_list, id1=".id", id2=NULL) {
  # pdr_list is expected to be a valid data_col list-col
  # elements[1] should be an element name in data_col (list)
  # elements[2] should be an element in var1, if var1 is tibble/list

  if(is.null(id1)) {
    stop("Parameter 'id1' is NULL.")
  }

  # This is probably not the best way to solve this task.
  # A better solution is to check the class of element 'id1'
  # in pdr_list and determine the result on that basis:
  # a vector, if id1 class is int, num, chr, lgl, etc
  # a list, if id1 class list, tibble, matrix, array, etc

  if(is.null(id2)) {
    if(id1 %in% c(".points", ".strokes", ".polygons", ".rgba", ".stencil")) {
      pdr_list |> 
        purrr::map(id1)
    } else {
      pdr_list |> 
        purrr::map(id1) |>     # Just the one element
        purrr::list_simplify(strict=FALSE) # Convert to int, chr, num, ..
        
    }
  } else {
    # Should probably check to see if id2 is in names(id1)
    pdr_list |>
      purrr::map(id1) |>
      purrr::map(~dplyr::pull(.x, id2)) |>
      purrr::list_simplify(strict=FALSE)
  }
}