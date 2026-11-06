#' Modify the points of pain drawings
#'
#' Performs an operation on the `.points` of each pdr_drawing in a pdr_list
#' and returns a pdr_list of the same length. It is thus suited for use inside
#' `mutate()`. See [pdr_match()] for the definitions of pdr_list and
#' pdr_drawing.
#'
#' @param pdr_list A pdr_list (see [pdr_match()]), e.g. the `pdr_data` column
#'   of a pdr_tibble.
#' @param op A string specifying the operation. Currently one of:
#'   * `"flipy"` (default) — flips the y-coordinates (`.y` becomes
#'     `.height - .y`), which is useful to move the origin between the top and
#'     bottom of the canvas. Drawings without `.points` are left unchanged.
#'
#' @returns A pdr_list of the same length as `pdr_list`, with the modified
#'   `.points`.
#'
#' @seealso [pdr_modify_rgba()]
#'
#' @export
#' @examples
#' # Flip the y-coordinates and store in a new pdr_list column
#' pdr_example_data |>
#'   dplyr::mutate(pdr_data_flipped = pdr_modify_points(pdr_data, "flipy"))
pdr_modify_points <- function(pdr_list, op = "flipy") {
  ########## Sanity checks ##########
  accepted_ops <- c("flipy")

  # Require 'op' to be a character vector of fixed options
  if (!is.character(op)) {
    stop(
      "Stopped: The function `pdr_modify_points()` expects a character vector as parameter 'op'."
    )
  }
  if (any(identical(NA, op)) || any(op == "")) {
    warning("Unknown op specified in function `pdr_modify_points()`.")
    return(pdr)
  }
  if (
    any(
      !{
        op %in% accepted_ops
      }
    )
  ) {
    warning("Unknown op specified in function `pdr_modify_points()`.")
  }

  ########## End sanity checks ##########

  ########## op functions ##########
  flipy <- function(pdr) {
    # This function flips (mirrors) the y-axis
    # This solves a common problem where different platforms
    # tend to use a traditional cartesion coordinates system
    # with origo at the _lower_ left whereas computer screens
    # typically place the origo in the _upper_ left

    pdr |>
      purrr::map(\(e) {
        if (is.list(e) && !is.null(e$.points)) {
          e$.points$.y <- e$.height - e$.points$.y
        }
        e # map return
      })
  }

  ########## End op functions ##########

  if ("flipy" %in% op) {
    pdr_list <- flipy(pdr_list)
  }

  return(pdr_list)
}
