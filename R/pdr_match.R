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
#' @returns Depending on input parameters. Will return TRUE/FALSE if action is set to 'eval' and as is specified. Will
#'   return the input pdr_data type (pdr_tibble, pdr_list og pdr_drawing) as a string if action is set to 'eval' and
#'   as is not set. Will return an actual pdr_tibble, pdr_list or pdr_drawing, if action is set to 'coerce'.
#'
#' @export
#' @examples
#' pdr_example_data |> pdr_match() # Returns "pdr_tibble"
#' 
#' pdr_example_data |> pdr_match(as="pdr_list") # Returns FALSE
#' 
#' pdr_eample_data$pdr_data |> pdr_match(action="coerce", as="pdr_tibble", mode="validate") # Returns a validated pdr_tibble with columns 'id' and `pdr_list` where the latter is named as the input.
pdr_match <- function(input, action="eval", as="", mode="superficial") {
  # This function takes as input a variable presumed to be a 
  # valid 'pdr_tibble', 'pdr_list' or 'pdr_drawing'.

  # Sanity checks
  if(!as %in% c("","pdr_list","pdr_tibble", "pdr_drawing")) {stop("Invalid 'as' parameter.")}
  if(!action %in% c("coerce","eval")) {stop("Invalid 'action' parameter.")}
  if(!mode %in% c("superficial","validate")) {stop("Invalid 'mode' parameter.")}
  
  # These are the required named elements in a pdr_drawing:
  required_elements <- 
    c(".id", ".points", ".strokes", ".width", ".height")

  # Helper functions
  is_valid_rgba <- function(v) {
    if(is.null(v)) {return(TRUE)} # v is not required

    # ..but if present:
    is_valid <- (
      is.array(v) &&
      is.numeric(v) &&
      length(dim(v)) == 3 &&
      dim(v)[3] == 4    
    )
    if(!is_valid) {warning("Invalid .rgba element")}
    return(is_valid)
  }

  ########## ATTEMPT COERCION ##########
  if(action == "coerce") {
    # First evaluate what we actually have as input...
    input_is <- pdr_match(input, action="eval", mode=mode)
    if(input_is == as) {
      # We already have what we want (tibble, list or drawing)
      return(input)
    } 
    if(input_is=="pdr_list" & as=="pdr_tibble") {
      return(
        tibble::tibble(id=input |> purrr::map_chr(".id"), pdr_data=input)
      )
    }
    if(input_is=="pdr_drawing" & as=="pdr_tibble") {
      return(
        tibble::tibble(id=input$.id, pdr_data=list(input))
      )
    }
    if(input_is=="pdr_drawing" & as=="pdr_list") {
      return(
        list(input)
      )
    }
    stop(paste0("The input appears to be ",input_is, " which 
    can not be coerced into ", as,"."))
  }
  ########## END ATTEMPT COERCION ##########



  ########## SUPERFICIAL MATCHING ##########
  if(action=="eval" && mode=="superficial") {
    if(tibble::is_tibble(input) && all(c("id","pdr_data") %in% names(input))) {
      if(as=="pdr_tibble") {return(TRUE)}
      else if(as=="") {return("pdr_tibble")}
      else {return(FALSE)}
    }
    if(is.list(input) && all(input |> purrr::map_lgl(is.list))) {
      if(as=="pdr_list") {return(TRUE)}
      else if(as=="") {return("pdr_list")}
      else {return(FALSE)}
    }
    if(is.list(input) && all(required_elements %in% names(input))) {
      if(as=="pdr_drawing") {return(TRUE)}
      else if(as=="") {return("pdr_drawing")}
      else {return(FALSE)}
    }
    return("invalid")  
  }
  ########## END SUPERFICIAL MATCHING ##########
  
  
  ########## VALIDATATION MATCHING #########
  if(action=="eval" && mode=="validate") {


    ##### JUST SIMPLY INVALID #####
    # If input is not a list or a tibble, it is invalid
    if(!is.list(input) & !tibble::is_tibble(input)) {
      if(action=="eval") return("invalid")
    }
    ##### END JUST SIMPLY INVALID #####

    
    ##### TIBBLE VALIDATATION MATCHING #####
    if(tibble::is_tibble(input)) {
      # When input is a tibble, we should check for one at least
      # one valid pdr_list column and match it as pdr_list:
      number_of_pdr_lists <- input |>
        dplyr::summarize_if(is.list, \(x) {
          pdr_match(x, action="eval", as="pdr_list", mode="validate")
        }) |> sum()

      if(number_of_pdr_lists > 0) {
          if(as=="pdr_tibble") {return(TRUE)}
          else if(as=="") {return("pdr_tibble")}
          else {return(FALSE)}
      } else {
          return("invalid")
      } 
    }
    ##### END TIBBLE #####


    ##### LIST VALIDATATION MATCHING #####
    if(is.list(input)){
      if(all(input |> purrr::map_lgl(is.list))) {
        # So input is a list of lists, iterate and check whether each is a valid pdr_drawing
        if(all(input |> purrr::map_lgl(\(x) {pdr_match(x, action="eval", as="pdr_drawing", mode="validate")}))) {
          # So all elements are pdr_drawings:
            if(as=="pdr_list") {return(TRUE)}
            else if(as=="") {return("pdr_list")}
            else {return(FALSE)}        
        } else {
          # So input is list of lists and thus not a drawing
          # but also not all elements are drawings, so invalid
          return("invalid") 
        }
      }
    }
    ##### END LIST ##### 


    ##### DRAWING VALIDATATION MATCHING #####
    if(is.list(input)) {
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
        setequal(input$.points$.index, input$.strokes$.index) &&
        is_valid_rgba(input$.rgba))
      {
        # It's a valid pdr_drawing, so do this:
          if(as=="pdr_drawing") {return(TRUE)} 
          else if(as=="") {return("pdr_drawing")}
          else {return(FALSE)}
      } else {
        # It is not a valid pdr_drawing, so do this:
        return("invalid")
      }
    }
  }

  return(NA) # We should never get to this line
########## END VALIDATATION MATCHING #########
}
