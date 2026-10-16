#' Argument verification of pain drawing data types
#' 
#' `pdr_match` matches an input parameter against the three
#' pain drawing data types:
#' 
#' * pdr_tibble -- A tidyverse tibble which must contain columns:
#'     - 'id' (type _chr_) of unique identifiers
#'     - 'pdr_data' (type _pdr_list_)
#' * pdr_list -- A tidyverse list of lists of named elements:
#'     - '.id' (type _chr_) of unique identifiers
#'     - '.width' (type _int_) of canvas x dimensions
#'     - '.height' (type _int_) of canvas y dimensions
#'     - '.points' (type _tibble_) of coordinate data (required: .x, .y, .index)
#'     - '.strokes' (type _tibble_) of stroke values (required: .index)
#'
#' If the parameter 'check_only' is TRUE, the function returns a string of 'pdr_tibble', 'pdr_list', 'pdr_drawing' or 'invalid'.
#' 
#' If the parameter 'mode' is "superficial" a quick and simple check of the structure of arg is done. If 'mode' is "validate", a more detailed check of data structure and content requirements is performed.
#' 
#' When the parameter 'as' is "list", the function will return a pdr_list if 'arg' is a pdr_drawing. Similarly, if 'as' is "tibble", an 'arg' which is a pdr_drawing or a pdr_list will be returned as a pdr_tibble, with columns 'id' and 'pdr_data'. When 'as' is specified, the function defauls to 'mode' = "validate" and 'check_only' = FALSE.
#'
#' @param arg A variable to be matched against 'pdr_tibble', 'pdr_list' and 'pdr_drawing' data types.
#' @param check_only A logical specifying whether to enforce match or return a result string
#' @param mode A string of 'validate' or 'superficial' specifying superficial matching or actual validation
#' @param as A string of "", "list" or "tibble", specifying whether to coerce a a pdr_drawing or pdr_list to pdr_list or pdr_tibble.
#'
#' @returns
#'
#' @export
#' @examples
pdr_match <- function(input, action="eval", as="", mode="validate") {
  # This function takes as input a variable presumed to be a 
  # valid 'pdr_tibble', 'pdr_list' or 'pdr_drawing'.

  # Sanity checks
  if(!as %in% c("","pdr_list","pdr_tibble", "pdr_drawing")) {stop("Invalid 'as' parameter.")}
  # If as is specified, always validate data:
  # if(as != "") {mode <- "validate"; check_only <- FALSE}
  if(!action %in% c("coerce","eval")) {stop("Invalid 'action' parameter.")}
  if(!mode %in% c("superficial","validate")) {stop("Invalid 'mode' parameter.")}
  
  
  # These are the required named elements in a pdr_drawing:
  required_elements <- 
    c(".id", ".points", ".strokes", ".width", ".height")

  ########## SUPERFICIAL MATCHING ##########
  if(mode=="superficial") {
    if(tibble::is_tibble(input) && all(c("id","pdr_data") %in% names(input))) {
      return("pdr_tibble")
    }
    if(is.list(input) && all(input |> purrr::map_lgl(is.list))) {
      return("pdr_list")
    }
    if(is.list(input) && all(required_elements %in% names(input))) {
      return("pdr_drawing")
    }
    return("invalid")
  }

  ########## END SUPERFICIAL MATCHING ##########
  
  ##########################################################

  # When we make it this far, mode is 'validate'

  ########## VALIDATATION MATCHING #########
  
  ##### INVALID #####
  # If input is not a list or a tibble, it is invalid in any case
  if(!is.list(input) & !tibble::is_tibble(input)) {
    if(action=="coerce") {stop("! 'input' is not a pdr_tibble, pdr_list or pdr_drawing and can not be coerced into such")}
    if(action=="eval") return("invalid")
  }
  ##### END INVALID #####

  # ..this far means input is either a tibble or a list

  ##### TIBBLE #####
  if(tibble::is_tibble(input)) {
    # When input is a tibble, we should check for one at least
    # one valid pdr_list column and macth it as pdr_list:
    num_pdr_lists <- input |>
      dplyr::summarize_if(is.list, \(x) {
        pdr_match(x, action="eval") == "pdr_list"
      }) |> sum()

    if(action=="eval") {
      if(num_pdr_lists > 0) {
        return(ifelse(as=="pdr_tibble", TRUE, "pdr_tibble"))
      } else {
        return("invalid")
      } 
    } 
    if(action=="coerce") {
      if(num_pdr_lists > 0) {
        return(input) # We can not coerce into anything
      } else {
        stop("! 'input' is a tibble with no valid pdr_list columns.")
      }
    }
  }
  ##### END TIBBLE #####

  # ..this far means input is a list

  ##### LIST #####
  if(all(input |> purrr::map_lgl(is.list))) {
    # When input is a list of lists, iterate and check whether each is a valid pdr_drawing
    if(all(input |> purrr::map_lgl(\(x) {
      pdr_match(x, action="eval", as="pdr_drawing")
    }))) {
      # When all elements are pdr_drawings:
      if(action=="eval") {
        return(ifelse(as=="pdr_list", TRUE, "pdr_list") )
      } else {  # action=="coerce"
        if(as == "pdr_tibble") {return(
          tibble::tibble(id=input |> purrr::map_chr(".id"), pdr_data=input)
        )} else {}
          return(input)
      }        
    } else {
      # When not all elements are pdr_drawing
      return(ifelse(action=="eval", "invalid", {stop("! 'input' is a list of lists, but contains non-pdr_drawing elements.")}))
    }
    }

  ##### END LIST ##### 

  # ..this far means input is a list but not list-of-lists

  ##### DRAWING #####
  if(
    !is.null(names(input)) && 
    all(required_elements %in% names(input)) && 
    is.character(input$.id) && 
    is.integer(input$.width) &&
    is.integer(input$.height) &&
    tibble::is_tibble(input$.points) &&
    tibble::is_tibble(input$.strokes) &&
    all(c(".x", ".y", ".index") %in% names(input$.points)) &&
    ".index" %in% names(input$.strokes) && 
    is.integer(input$.points$.x) &&
    is.integer(input$.points$.y) &&
    is.integer(input$.points$.index) &&
    is.integer(input$.strokes$.index) &&
    setequal(input$.points$.index, input$.strokes$.index)
   ) {
    # It's a valid pdr_drawing, so do this:
    if(action=="eval") {
      if(as == "pdr_drawing") {
        return(TRUE)
      } else {
        return("pdr_drawing")
      }
    } else { # action == "coerce"
      if(as == "pdr_tibble") { 
        return(tibble::tibble(id=input$.id, pdrdata=list(input)))
      } else if(as == "pdr_list") {
        return(list(input))
      } else {
        return(input)
      }
    }
  } else {
    # It is not a valid pdr_drawing, so do this:
    return(ifelse(action=="eval", "invalid", stop(paste0("! 'input' is like a pdr_drawing, but one or more of ",required_elements, " is missing og wrong data type."))))  
  }

  return(NA) # We should never get to this line
########## END VALIDATATION MATCHING #########
}
