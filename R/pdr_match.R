#' Check or coerce pain drawing data types
#'
#' `pdr_match()` matches an input against the three pain drawing data types
#' used throughout paindrawr, and either reports which type it is (or whether
#' it is a given type), or coerces it to another type.
#'
#' @section Terminology:
#' This is where the paindrawr data types are defined; all other functions
#' refer here.
#'
#' | Term | Meaning |
#' |---|---|
#' | pdr_drawing | A named list for one drawing, with elements `.id`, `.file`, `.version`, `.width`, `.height`, `.units`, `.timestamp`, `.app`, `.strokes`, `.points`, and optionally `.rgba`, `.stencil` and `.stencil_inverted` |
#' | pdr_list | A list of pdr_drawings (typically the list-column `pdr_data`) |
#' | pdr_tibble | A tibble with columns `id` and a pdr_list column (default name `pdr_data`) |
#'
#' Required elements of a pdr_drawing:
#' * `.id` (character): unique identifier
#' * `.width`, `.height` (integer): canvas dimensions
#' * `.points` (tibble): coordinates (required columns `.x`, `.y`, `.index`,
#'   all integer)
#' * `.strokes` (tibble): stroke values (required column `.index`, integer)
#'
#' Optional: `.rgba` (numeric array with dimensions height x width x 4).
#'
#' @section Matching:
#' With `action = "eval"` (default) and `as = ""`, the function returns a
#' string: `"pdr_tibble"`, `"pdr_list"`, `"pdr_drawing"` or `"invalid"`. With
#' `action = "eval"` and `as` set to one of the types, it returns `TRUE` or
#' `FALSE` for whether `input` is that type (an `input` that is `NA` or `NULL`
#' gives `FALSE`).
#'
#' With `action = "coerce"`, `as` must be given and the function returns
#' `input` as that type. Supported conversions: pdr_list to pdr_tibble,
#' pdr_drawing to pdr_tibble, and pdr_drawing to pdr_list. If `input` already
#' is of the requested type it is returned unchanged; any other conversion, or
#' an `NA`/`NULL` input, is an error.
#'
#' @section Modes:
#' With `mode = "superficial"` (default) a quick check of the structure of
#' `input` is made (a pdr_tibble has columns `id` and `pdr_data`; a pdr_list
#' is a list of lists; a pdr_drawing is a list with the required elements).
#' With `mode = "validate"`, a more detailed check of the element types and of
#' the contents of `.points`, `.strokes` and `.rgba` is made (for a tibble, at
#' least one list column must be a valid pdr_list). Invalid `.rgba` gives a
#' warning.
#'
#' @param input A variable to be matched against the pdr_tibble, pdr_list
#'   and pdr_drawing types.
#' @param action `"eval"` (default) to evaluate the type of `input`, or
#'   `"coerce"` to convert `input` to the type given in `as`.
#' @param as One of `""` (default), `"pdr_list"`, `"pdr_tibble"` or
#'   `"pdr_drawing"`. For `action = "eval"`, the type to test for (`""` returns
#'   the type as a string). For `action = "coerce"`, the type to coerce to.
#' @param mode `"superficial"` (default) for a quick structural check, or
#'   `"validate"` for a detailed check of structure and content.
#'
#' @returns For `action = "eval"`: a string (the type of `input`, or
#'   `"invalid"`) if `as = ""`, otherwise `TRUE`/`FALSE`. For
#'   `action = "coerce"`: `input` as a pdr_tibble, pdr_list or pdr_drawing,
#'   as requested in `as`.
#'
#' @export
#' @examples
#' pdr_example_data |> pdr_match() # "pdr_tibble"
#'
#' pdr_example_data |> pdr_match(as = "pdr_tibble") # TRUE
#'
#' pdr_example_data |> pdr_match(as = "pdr_list") # FALSE
#'
#' pdr_example_data$pdr_data |> pdr_match() # "pdr_list"
#'
#' pdr_example_data$pdr_data[[1]] |> pdr_match() # "pdr_drawing"
#'
#' # Coerce a pdr_list to a pdr_tibble with columns `id` and `pdr_data`
#' pdr_example_data$pdr_data |>
#'   pdr_match(action = "coerce", as = "pdr_tibble", mode = "validate")
pdr_match <- function(input, action="eval", as="", mode="superficial") {
  # This function takes as input a variable presumed to be a 
  # valid 'pdr_tibble', 'pdr_list' or 'pdr_drawing'.

  ######### SANITY CHECKS ########
  if(identical(NA,input) || is.null(input)) {
    if(action=="eval") {return(FALSE)}
    if(action=="coerce") {stop("Invalid 'input' parameter.")}
  }
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
