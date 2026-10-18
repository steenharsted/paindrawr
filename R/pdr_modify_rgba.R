#' Modify rgba data stored in the pain drawing list column
#' 
#' @param pdr_list
#'
#' @returns
#'
#' @export
#' @examples
pdr_modify_rgba <- function(pdr_list, op="", stencil=NULL, threshold = 0.5) {
  # Sanity checks -- see stop message for explanation
  if(!op %in% c("multiply", "addition", "apply_stencil", "low_trim","high_trim")) { stop("Parameter 'op' has invalid value.")}
  if(!is.numeric(threshold) | threshold<0 | threshold>1) {stop("Parameter 'threshold' has invalid value.")}
  pdr_list <- pdr_match(pdr_list, action="coerce", as="pdr_list")

  
  #### Now coerce the stencil parameter to a list of matrices:
  # stencil can be a pdr_list with a .stencil element, or
  # stencil can be a single matrix, or a list of matrices 
  if(!is.null(stencil)){
    if(is.matrix(stencil)) { 
      # stencil is a matrix so:
      # make stencil a list of length 1 (unnamed) with a matrix
      stencil <- list(stencil)
    } else if(is.list(stencil) && all(stencil |> purrr::map_lgl(\(e_stencil) {is.matrix(e_stencil)}))) {
      # stencil is a list of matrices so:
      # leave stencil as is
    } else if(pdr_match(stencil, action="eval", as="pdr_list") &&
              all(stencil |> purrr::map_lgl(\(e_stencil) {".stencil" %in% names(e_stencil)}))) {
      # stencil is a list of lists with named elements '.stencil. so:
      # make stencil a list of the .rgba matrices
      stencil <- stencil |> purrr::map(".stencil")
    } else {
      stop("Parameter 'stencil' must be a) a matrix, b) a list of matrices or c) pain drawing list column with .stencil elements.")
    }
  
    # Sanity check for array/matrix x,y dimentions?
    # Further sanity checks?  -- see stop message for explanation
    # if(length(stencil)!=1 && length(stencil)!=length(pdr_list)) {
    #   stop("Parameter stencil must be coercible to a list of matrices of length 1 or same length as parameter pdr_list")
    # } 
    # if(!all(purrr::map2_lgl(pdr_list, stencil, \(e_pl, e_stencil) {identical(dim(e_pl$.rgba), dim(e_stencil))}))) {
    #   stop("Matrix dimensions of pdr_list and stencil do not match pairwise.")
    # }
  }
  
  #### This function can modify the .rgba data stored in
  # the pain drawing list column, e.g. by multiplying or
  # adding rgba matrices of the same dimensions -- this is
  # useful for a) applying templates (multiplying with matrix
  # of 0 and 1 values) and b) creating weighted heat maps 
  # (addition of 0-1 numeric value matrix).
  #
  # As pdr_list is a list of length n, the stencil parameter must
  # be either a list of length n of matrices or a single matrix
  
  if(op=="multiply") {
    purrr::map2(pdr_list, stencil, \(e_pl, e_stencil) {
      e_pl$.rgba <- e_pl$.rgba * e_stencil
    })
  } else if(op=="addition") {
    #################################################
    #### NOTE THIS MAY REQUIRE SPECIAL WEIGHTING ####
    ####            ASK STEEN ...                ####
    #################################################
    purrr::map2(pdr_list, stencil, \(e_pl, e_stencil) {
      e_pl$.rgba <- e_pl$.rgba + e_stencil
    })
  } else if(op=="low_trim") {
      pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
        pdr_drawing$.rgba <-  ifelse(pdr_drawing$.rgba<threshold,0,pdr_drawing$.rgba)
        pdr_drawing
      })
  } else if(op=="high_trim") {
      pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
        pdr_drawing$.rgba <-  ifelse(pdr_drawing$.rgba>threshold,1,pdr_drawing$.rgba)
        pdr_drawing
      })
  } 
  return(pdr_list)
}
