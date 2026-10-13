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
#' If the para
#'
#' @param arg a variable to be matched against 'pdr_tibble', 'pdr_list' and 'pdr_drawing' data types.
#' @param check_only a logical specifying whether to enforce match or return a result string
#' @param mode a string of 'validate' or 'superficial' specifying superficial matching or actual validation
#'
#' @returns
#'
#' @export
#' @examples
pdr_match <- function(arg, check_only=FALSE, mode="validate") {
  # This function takes as input a variable presumed to be a 
  # valid 'pdr_list' (i.e. a list of pdr_drawing elements) 
  # or a single 'pdr_drawing' element (i.e. a list of named 
  # elements of which some are required).

  # The function halts execution if pdr_v is invalid and 
  # check_only is FALSE.

  # The function works by testing for pdr_tibble first
  # Only if arg is not a tibble, we test for list (list_pdr)
  # Then if arg is not a list of lists we test for drawing

  if(!is.logical(check_only)) {warning("! 'check_only* should be TRUE or FALSE"); stop()}
  mode <- match.arg(mode, choices = c("validate", "superficial"))
  # Specify required named elements in a pdr_drawing:
  required_elements <- 
    c(".id", ".points", ".strokes", ".width", ".height")

  ########## SUPERFICIAL MATCHING ##########
  if(mode=="superficial") {
    if(tibble::is_tibble(arg) && all(c("id","pdr_data") %in% names(arg))) {
      return(ifelse(check_only, "pdr_tibble", arg))
    }
    if(is.list(arg) && all(arg |> purrr::map_lgl(is.list))) {
      return(ifelse(check_only, "pdr_list", arg))
    }
    if(is.list(arg) && all(required_elements %in% names(arg))) {
      return(ifelse(check_only, "pdr_drawing", arg))
    }
    ifelse(check_only,"invalid", {warning("! 'arg' is not a valid pain drawing data type"); stop()})
  }

  ########## END SUPERFICIAL MATCHING ##########
  
  ##########################################################

  ########## VALIDATATION MATCHING #########
  
  ##### INVALID #####
  # If arg is not a list or a tibble, it is invalid in any case
  if(!is.list(arg) & !tibble::is_tibble(arg)) {
    if(check_only) {
      return("invalid")
    } else {
      warning(
        paste0("! 'arg' should be one of list-data types 'pdr_tibble', 'pdr_list' or 'pdr_drawing', not ", class(arg)))
      stop()
    }
  }
  ##### END INVALID #####

  # ..this far means arg is either a tibble or a list

  ##### TIBBLE #####
  if(tibble::is_tibble(arg)) {
    # When it is a tibble, we should check for valid pdr_list 
    # columns. We count list-cols that macth pdr_list:
    n_pdr_lists <- arg |> dplyr::summarize_if(is.list, \(c) {pdr_match(c, check_only=TRUE, mode="validate") == "pdr_list"}) |> sum()
    if(check_only) {
      if(n_pdr_lists > 0) {
        return("pdr_tibble")
      } else {
        return("invalid")
      } 
    } else {
      if(n_pdr_lists > 0) {
        return(arg)
      } else {
        warning("! 'arg' is a tibble with no valid pdr_list columns.")
        stop()
      }
    }
  }
  ##### END TIBBLE #####

  # ..this far means arg is a list

  ##### LIST #####
  if(all(arg |> purrr::map_lgl(is.list))) {
    # When arg is a list of lists, check whether it is a valid pdr_list
    if(all(arg |> purrr::map_lgl(\(x) {pdr_match(x, check_only=TRUE, mode="validate") == "pdr_drawing"}))) {
      # When all elements are pdr_drawings
      return(ifelse(check_only, "pdr_list", arg))
    } else {
      # When not all elements are pdr_drawing
      return(ifelse(check_only, "invalid", {stop("! 'arg' is a list of lists, but contains non-pdr_drawing elements.")}))
    }
  }

  ##### END LIST ##### 

  # ..this far means a list but not list-of-lists

  ##### DRAWING #####
  if(is.null(names(arg)) || length(setdiff(required_elements, names(arg)))>0)  {
    # When all or some required names are missing from arg
    return(ifelse(check_only, "invalid", stop(paste0("! 'arg' is like a pdr_drawing, but missing some of ",required_elements))))    
  } else {
    # When all required names are present, check whether element data types okay
    if(
        is.character(arg$.id) && 
        is.integer(arg$.width) &&
        is.integer(arg$.height) &&
        tibble::is_tibble(arg$.points) &&
        tibble::is_tibble(arg$.strokes) &&
        all(c(".x", ".y", ".index") %in% names(arg$.points)) &&
        all(c(".index") %in% names(arg$.strokes)) &&# Should we test data type of .x, .y and .index?
        setequal(arg$.points$.index, arg$.strokes$.index)
      ) {
      return(ifelse(check_only, "pdr_drawing", arg))
    } else {
      return(ifelse(check_only, "invalid", stop("! 'arg' is like a pdr_drawing, but with wrong data types or corrupted .strokes and .points.")))
    }
  }

  return(NA) # We should never get to this line
########## END VALIDATATION MATCHING #########
}
