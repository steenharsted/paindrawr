#' Check if data is valid pain drawing
#'
#' This function checks whether a variable is a valid pain drawing data structure
#'
#' A valid pain drawing data structure must adhere to these criteria:
#' * It should be a list, with three named elements (preferrably in this order):
#'   - "drawings" which is a dataframe/tibble which must include:
#'     * "id" which is a <chr> and represents a unique identifier of the pain drawing (only present in one row)
#'   - "strokes" which is a dataframe/tibble which must include:
#'     * "id"  which is a <chr> and references a corresponding "id" in "drawings" (may be present in multipe rows)
#'     * "i"  which is an <int> and represents a unique identifier of the stroke/marking (only present in one row)
#'   - "points" which is a dataframe/tibble which must include:
#'     * "id" which is a <chr> and references a corresponding "id" in "drawings" (may be present in multipe rows)
#'     * "i" which is an <int> and references a corresponding "i" in "strokes" (may be present in multipe rows)
#'     * "x"  which is an <int> and represents an x coordinate
#'     * "y"  which is an <int> and represents a y coordinate
#'
#' All three elements (dataframes) of the pain drawing data structure may contain any other number of columns. In fact, some functions will create new columns in the returned data structure, such as e.g. areas calculated for strokes/markings and summed area for pain drawings, by [pd_geom_area].
#'
#' @param pd A data structure to be checked
#'
#' @returns TRUE or FALSE
#'
#' @export
#' @examples
pd_check <- function(pd) {
  if (!is.list(pd)) {
    return(FALSE)
  }
  if (!identical(names(pd), c("drawings", "strokes", "points"))) {
    return(FALSE)
  }
  if (!"id" %in% all_of(names(pd$drawings))) {
    return(FALSE)
  }
  if (!"id" %in% all_of(names(pd$strokes))) {
    return(FALSE)
  }
  if (!"i" %in% all_of(names(pd$strokes))) {
    return(FALSE)
  }
  if (!"id" %in% all_of(names(pd$points))) {
    return(FALSE)
  }
  if (!"i" %in% all_of(names(pd$points))) {
    return(FALSE)
  }
  if (!"x" %in% all_of(names(pd$points))) {
    return(FALSE)
  }
  if (!"y" %in% all_of(names(pd$points))) {
    return(FALSE)
  }
}
