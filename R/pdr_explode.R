#' Explode pain drawings into multiple drawings by stroke
#'
#' Expodes each pain drawing into multiple drawings such that each stroke
#' (as defined by the `.strokes` and `.points` list-columns in input data) 
#' becomes a new separate pain drawing.
#' 
#' **NOTE** The output of this function is (likely) not of same length as input.
#'
#' @details
#' For each input element (pain drawing):
#'
#' * Each stroke in the `.strokes` column becomes a new pain drawing element
#' * The matching coordinates in the `.points` column are retained
#' * The .id column is modified -- see below.
#'
#' The output preserves the structure of a valid pain drawing data structure
#' (see [pdr_check_data()]), but with a different number of pain 
#' drawings. The function is thus not suited for `mutate()` and similar functions 
#' which expect the same length of input and output.
#'
#' **Creation of id string**
#' 
#' To ensure unique id strings for all observations, the `.id` element of
#' each pain drawing in the result is prepended by the list index value of 
#' the input pain drawing data and appended by the stroke index.
#'
#' **Handling of missing or empty data**
#'
#' * If `.strokes` or `.points` is `NA` or empty, the corresponding output 
#' pain drawings rows will contain `NA` in those columns
#'
#' @param pd A valid pain drawing data structure (see [pdr_check_data()]).
#'
#' @returns
#' A list representing a valid pain drawing data structure where each element
#' corresponds to a pain drawing with a single stroke from the input.
#'
#' @export
#'
#' @examples
#' # Explode strokes into separate rows
#' pdr_exploded <- pdr_explode(pdr_demo_data)
#'
#' # Inspect how many strokes per original drawing
#' pdr_demo_data |>
#'   dplyr::count(id)
#' pdr_exploded |>
#'   dplyr::count(id)
#'
#' # Each row now contains exactly one stroke
#' pdr_exploded$p[[1]]



pdr_explode <- function(pdr) {

  # This function takes pain drawing data as input.
  # It explodes/expands each pain drawing (row) into
  # several new pain drawings/row -- one for each stroke



  result <- pdr |>

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
