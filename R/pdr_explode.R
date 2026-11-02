#' Explode pain drawings into multiple drawings by stroke
#'
#' Explodes each pdr_drawing into multiple pdr_drawings such that each stroke
#' (as defined by the `.strokes` and `.points` elements of the drawing)
#' becomes a new separate pdr_drawing.
#'
#' **NOTE** The output of this function is (likely) not of same length as input.
#'
#' @details
#' For each input pdr_drawing:
#'
#' * Each row in `.strokes` becomes a new pdr_drawing
#' * The matching rows (same `.index`) of `.points` are retained
#' * The `.id` element is modified -- see below.
#'
#' The output is a pdr_list (see [pdr_match()]) with a different number of
#' pdr_drawings than the input. The function is thus not suited for `mutate()`
#' and similar functions which expect the same length of input and output.
#'
#' **Creation of id string**
#'
#' To ensure unique id strings for all pdr_drawings, the new `.id` has the
#' format `<index>_<id>_<stroke>`, where `<index>` is the position of the
#' input pdr_drawing in `pdr_list`, `<id>` is its original `.id`, and
#' `<stroke>` is the stroke's `.index`. E.g. `"2_abc_3"`.
#'
#' **Handling of missing or empty data**
#'
#' A pdr_drawing without strokes (`.strokes` is `NA`, `NULL` or has no rows)
#' contributes no pdr_drawings to the result.
#'
#' @param pdr_list A pdr_list (see [pdr_match()]), e.g. the `pdr_data`
#'   column of a pdr_tibble.
#'
#' @returns
#' A pdr_list where each element is a pdr_drawing containing a single stroke
#' from the input.
#'
#' @export
#'
#' @examples
#' # Explode a pdr_list into one pdr_drawing per stroke
#' pdr_exploded <- pdr_explode(pdr_example_data$pdr_data[1:3])
#'
#' # Number of strokes per original drawing vs. number of exploded drawings
#' sapply(pdr_example_data$pdr_data[1:3], \(d) nrow(d$.strokes))
#' length(pdr_exploded)
#'
#' # The new ids have the format <index>_<id>_<stroke>
#' pdr_get_info(pdr_exploded, ".id")[1:5]
#'
#' # Each pdr_drawing now contains exactly one stroke
#' nrow(pdr_exploded[[1]]$.strokes)
pdr_explode <- function(pdr_list) {

  # This function takes pain drawing data as input.
  # It explodes/expands each pain drawing (row) into
  # several new pain drawings/row -- one for each stroke



  result <- pdr_list |>

  purrr::imap(\(obj, indx) {

    # all stroke indices
    idx <- obj$.strokes$.index

    purrr::map(idx, \(i) {

      # copy original object
      out <- obj

      # make unique id
      out$.id <- paste0(indx, "_", obj$.id, "_", i)

      # keep only matching stroke row and points
      if(identical(NA, obj$.strokes) || nrow(obj$.strokes)==0) {
        out$.strokes <- NA 
        out$.points <- NA
      } else {
        out$.strokes <- obj$.strokes |>
          dplyr::filter(.index == i)
        out$.points <- obj$.points |>
          dplyr::filter(.index == i)
      }

      out
      })

  }) |>

  purrr::list_flatten()


  return(result)
  
}
