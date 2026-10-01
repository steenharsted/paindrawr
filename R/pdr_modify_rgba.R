#' Modify rgba data stored in the pain drawing list column
#' 
#' @param pdr_list
#'
#' @returns
#'
#' @export
#' @examples
pdr_modify_rgba <- function(pdr_list, op="", mx2=NULL) {
  # Sanity checks -- see stop message for explanation
  if(op=="" || is.null(mx2)) {
    stop("Parameters 'op' and 'mx2' must be passed to 'pdr_modify_rgba'.")
  }

  if(op!="multiply" & op!="addition") {
    stop("Parameters 'op' must be 'multiply' or 'addition'.")
  }

  if(!is.list(pdr_list) || 
            !all(pdr_list |> purrr::map_lgl(\(e_pl) {is.list(e_pl)})) ||
            !all(pdr_list |> purrr::map_lgl(\(e_pl) {".rgba" %in% names(e_pl)})) ||
            !all(pdr_list |> purrr::map_lgl(\(e_pl) {is.matrix(e_pl$.rgba)}))) {
    stop("Parameter pdr_list must be a list of lists with named elements '.rgba. which are all matrices.")
  }

  #### Now coerce the mx2 parameter to a list of matrices:
  # mx2 can be a single matrix, a list of matrices or a
  # pain drawing list col with a .rgba element (as mx2)
  if(is.matrix(mx2)) { 
    # mx2 is a matrix so:
    # make mx2 a list of length 1 with a matrix
    mx2 <- list(mx2)
  } else if(is.list(mx2) && all(mx2 |> purrr::map_lgl(\(e_mx2) {is.matrix(e_mx2)}))) {
    # mx2 is a list of matrices so:
    # leave mx2 as is
  } else if(is.list(mx2) && 
            all(mx2 |> purrr::map_lgl(\(e_mx2) {is.list(e_mx2)})) &&
            all(mx2 |> purrr::map_lgl(\(e_mx2) {".rgba" %in% names(e_mx2)}))) {
    # mx2 is a list of lists with named elements '.rgba. so:
    # make mx2 a list of the .rgba matrices
    mx2 <- mx2 |> purrr::map(".rgba")
  } else {
    stop("Parameter mx2 must be a) a matrix, b) a list of matrices or c) pain drawing list column with .rgba elements.")
  }
  
  # Further sanity checks -- see stop message for explanation
  if(is.list(mx2)) {
    if(length(mx2)!=1 & length(mx2)!=length(pdr_list)) {
      stop("Parameter mx2 must be coercible to a list of matrices of length 1 or same length as parameter pdr_list")
    }
  }
 
  if(!all(purrr::map2_lgl(pdr_list, mx2, \(e_pl, e_mx2) {identical(dim(e_pl$.rgba), dim(e_mx2))}))) {
    stop("Matrix dimensions of pdr_list and mx2 do not match pairwise.")
  }

  
  #### This function can modify the .rgba data stored in
  # the pain drawing list column, e.g. by multiplying or
  # adding rgba matrices of the same dimensions -- this is
  # useful for a) applying templates (multiplying with matrix
  # of 0 and 1 values) and b) creating weighted heat maps 
  # (addition of 0-1 numeric value matrix).
  #
  # As pdr_list is a list of length n, the mx2 parameter must
  # be either a list of length n of matrices or a single matrix
  
  if(op=="multiply") {
    purrr::map2(pdr_list, mx2, \(e_pl, e_mx2) {
      e_pl$.rgba <- e_pl$.rgba * e_mx2
    })
  } else if(op=="addition") {
    #################################################
    #### NOTE THIS MAY REQUIRE SPECIAL WEIGHTING ####
    ####            ASK STEEN ...                ####
    #################################################
    purrr::map2(pdr_list, mx2, \(e_pl, e_mx2) {
      e_pl$.rgba <- e_pl$.rgba + e_mx2
    })
  }
}
