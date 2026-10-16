#' Create and add a stencil to pain drawings
#' 
#' Pain drawings can be used as stencils and applied to 
#' other pain drawings. This is typically useful, when a 
#' particular region of a pain drawing is of particular 
#' interest. E.g. when only a specific anatomical regions of 
#' the pain drawing is relevant.
#' 
#' This function reduces a pain drawing to a stencil -- ie. 
#' the RGBA data is reduced to a single level (alpha) with 
#' just 0 and 1 values.
#'
#' @param pdr_list A valid pdr_list -- see `pdr_match()`
#' @param threshold A threshold value which determines a 
#' pixel as being 0 or 1
#'
#' @returns A pdr_list with added .stencil element which is 
#' a matrix of dimensions width x height with values 0 or 1
#'
#' @export
#' @examples
pdr_add_stencil <- function(pdr_list, threshold) {
  # NOTE. This function is based on any one of the
  # R, G or B levels/channels of the .rgba array being 
  # smaller than the threshold value (i.e darker) ... if 
  # that is the case for a given pixel, it is represented as
  # a value of 1 in the .stencil, otherwise as 0.

  # Sanity checks
  # Is pdr_list a valid pdr_list or can it be coerced into such?
  pdr_list <- pdr_match(pdr_list, action = "coerce", as="pdr_list", mode="superficial")
  # Is .rgba an element in all pdr_drawing elements of pdr_list
  if(!all(pdr_list |> purrr::map_lgl(\(pdr_drawing) {".rgba" %in% names(pdr_drawing)}))) {
    stop("Not all elements of the parameter 'pdr_list' contain a .rgba element")
  }

  pdr_list |> purrr::map(\(pdr_drawing) {
    pdr_drawing$.stencil <- (
      pdr_drawing$.rgba[,,1] < threshold |
      pdr_drawing$.rgba[,,2] < threshold |
      pdr_drawing$.rgba[,,3] < threshold
    ) + 0 # Make it integer
    pdr_drawing # map return
  })
}