#' Extract information from pain drawing objects
#'
#' Extracts metadata or variables from a pain drawing (`data_col`) list-column.
#' Simple elements (such as `".id"` or `".timestamp"`) are returned as a vector,
#' whereas columns within the nested `.strokes` or `.points` tibbles are
#' returned as a list with one element per pain drawing.
#'
#' @param pdr_list A valid pain drawing list-column -- see pdr_match for details.
#' @param id1 A string specifying which element of pdr_list to get
#' @param id2 A string specifying which element of id1 to get, assuming id1 is a tibble or list element of pdr_list
#'
#' @return
#' If vector of same length as pdr_list consisting of the specified elements
#'
#' @examples
#' # Extract ids
#' pdr_example_data |> mutate(id = pdr_get_info(pdr_data, ".id"))
#'
#' # Extract x coordinates from the .points tibble
#' pdr_example_data |> mutate(x_coord = pdr_get_info(pdr_data, ".strokes", ".x"))
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
      purrr::map(~dplyr::pull(.x, id2)) 
  }
}