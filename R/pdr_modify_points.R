
#' Modify pain drawing elements
#' 
#' This function takes a valid pain drawing list as input
#' and performs one or more operations sequentially on those 
#' data before returning a valid pain drawing list-column of 
#' the same length. It is thus suited for `mutate()` operations.
#' 
#' 
#' @param pdr_list The pain drawing data list-col in .data (defaults to pdr_data) 
#' @param ops A string specifying operation. See below:
#' 
#' The parameter `ops` can take one of the following values:
#' * "flipy" -- flips the y-coordinates which can be useful to re-locate origo to/from top/bottom of canvas.
#'
#' @returns A valid pain drawing data list column
#'
#' @export
#' @examples
#' # Flip the y-coordinates and store in a new pain drawing list-col
#' pdr_example_data |> mutate(pdr_data_flipped = pdr_mutate(pdr_data, "flipy"))

pdr_modify_points <- function(pdr_list, ops="flipy") {

  ########## Sanity checks ##########
  accepted_ops <- c("flipy")

  # Require 'ops' to be a character vector of fixed options
  if(!is.character(ops)) {
    stop("Stopped: The function `pdr_modify()` expects a character vector as parameter 'ops'.")
  } 
  if(any(identical(NA, ops)) || any(ops == "")) {
    warning("Unknown ops specified in function `pdr_modify()`.")
    return(pdr)
  }
  if(any(!{ops %in% accepted_ops})) {
    warning("Unknown ops specified in function `pdr_modify()`.")
  }
  
  ########## End sanity checks ##########

  ########## ops functions ##########
  flipy <- function(pdr) {
      # This function flips (mirrors) the y-axis
      # This solves a common problem where different platforms
      # tend to use a traditional cartesion coordinates system
      # with origo at the _lower_ left whereas computer screens
      # typically place the origo in the _upper_ left

      pdr |> 
        purrr::map(\(e) {
          if(!identical(NA, e$.points) && !is.null(e$.points)) {
            e$.points$.y <- e$.height - e$.points$.y
          }
          e # map return
        })
  }

  ########## End ops functions ########## 


  if("flipy" %in% ops) {
    pdr_list <- flipy(pdr_list)
  }

  return(pdr_list)
}