#' Create and add a stencil to pain drawings.
#' 
#' Users should only add a stencil to pain drawings that are 
#' intended to serve as stencils for other pain drawings. To 
#' apply stencils see `pdr_modify_rgba()`.
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
#' The 'threshold' parameter determines the threshold at 
#' which a pixel's value is dichotomized to 0 or 1 (FALSE or 
#' TRUE) as per the 'by' parameter. E.g. a pixel with alpha 
#' value 0.4 will evaluate to 0 by '>' a 'threshold' of 0.5
#' 
#' The 'base' parameter determines which of the R, G, B and 
#' A channels should be evaluated in reference to the 
#' threshold. E.g. 'RGBA' evaluates all of the channels, and
#' 'A' evaluates only the alpha channel.
#'
#' @param pdr_list A valid pdr_list -- see `pdr_match()`
#' @param threshold A threshold value which determines a 
#' pixel as being 0 or 1
#' @param base A string specifying which channels of the 
#' RGBA matrix to compare to the threshold. 
#' @param by Either '<' or '>'
#'
#' @returns A pdr_list with added .stencil element which is 
#' a matrix of dimensions width x height with values 0 or 1
#'
#' @export
#' @examples
pdr_add_stencil <- function(pdr_list, threshold=1, base="A", by = "<") {
  # NOTE. This function is based on any one of the
  # R, G, B or A levels/channels of the .rgba array being 
  # smaller than the threshold value (i.e darker) ... if 
  # that is the case for a given pixel, it is represented as
  # a value of 1 in the .stencil, otherwise as 0.

  # Sanity checks
  if(by!="<" & by!=">") {stop("Parameter 'by' is invalid")}
  # Is pdr_list a valid pdr_list or can it be coerced into such?
  pdr_list <- pdr_match(pdr_list, action = "coerce", as="pdr_list", mode="superficial")
  # Is .rgba an element in all pdr_drawing elements of pdr_list
  if(!all(pdr_list |> purrr::map_lgl(\(pdr_drawing) {".rgba" %in% names(pdr_drawing)}))) {
    stop("Not all elements of the parameter 'pdr_list' contain a .rgba element")
  }

  R <- stringr::str_detect(base, "R")
  G <- stringr::str_detect(base, "G")
  B <- stringr::str_detect(base, "B")
  A <- stringr::str_detect(base, "A")
  if(!any(R, G, B, A)) {stop("Invalid parameter 'base'. ")}

  # This code is a little opaque, but
  # (A && get(by)(pdr_drawing$.rgba[,,4] , threshold))
  # evaluates to TRUE if A is in 'base' parameter AND 
  # pdr_drawing$.. is > than threshold, or < than threshold
  # where < or > is determined by the 'by' parameter
  pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
    pdr_drawing$.stencil <- (
      (A & get(by)(pdr_drawing$.rgba[,,4] , threshold)) |
      (R & get(by)(pdr_drawing$.rgba[,,1] , threshold)) |
      (G & get(by)(pdr_drawing$.rgba[,,2] , threshold)) |
      (B & get(by)(pdr_drawing$.rgba[,,3] , threshold)) 
    ) + 0 # Make it integer
    pdr_drawing # map return
  })

  pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
    pdr_drawing$.stencil_inverted <- 
      (pdr_drawing$.stencil == FALSE) + 0 # Make it integer
    pdr_drawing # map return
  })
  pdr_list # func return
}