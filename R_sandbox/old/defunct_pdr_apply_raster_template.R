#' Title
#'
#' @param r1 Either a list of 2 dimentional matrices, or a 
#' valid column name from the r1_data parameter
#' @param r2 A list (of same length as r1 or length == 1) 
#' of 2 dimentional matrices, pairwise of the same 
#' dimentions as those of r1
#' @param invert If TRUE, the inverse template is applied
#' @param r1_data An optional valid pain drawing data tibble
#'
#' @returns A list (of same length as r1) of r1 of resulting
#' 2 dimensional matrices
#'
#' @export
#' @examples
pdr_apply_raster_template <- function(r1, r2, invert=FALSE, r1_data = NULL) {
  # Allow users to pass a column name in r1_data as r1.
  if(!is.null(r1_data)) {
    r1 <- {{r1_data}} |> dplyr::pull({{r1}})
  }
  # Allow users to submit r2 as a singlematrix
  if(!is.list(r2) && is.matrix(r2)) {
    warning("Converted the r2 parameter to list of length 1.")
    r2 <- list(r2)
  }
  
  # ---- sanity checks ----
  # If r1 or r2 is empty
  if(!length(r1)>0 || !length(r2)>0) {
    warning("Empty list passed as r1 or r2.")
    return(NA)
  }
  # Check lists are of same length (or r2 is length 1)
  if(length(r2)>1 && length(r1) != length(r2)) {
    warning(paste0(
      "Can't recycle `r2` (size ",length(r2), 
      ") to match r1 (size ", length(r1), ")."))
    return(NA)
  }
  # Check for matching matrix dimensions between r1 and r2
  mismatched_dimensions <- 
    !{purrr::map2_lgl(r1, r2, \(rr1, rr2) {identical(dim(rr1), dim(rr2))})}
  if(any(mismatched_dimensions)) {
    warning(paste0(
      "Mismatch in r1 vs r2 matrix dimensions at ", which(mismatched_dimensions)))
    return(NA)
  }

  if(invert) {
    r2 <- r2 |> purrr::map(\(rr2) {+!rr2}) # + coerces back to 0/1
  }

  purrr::map2(r1, r2, \(rr1,rr2) {rr1*rr2})

  
}