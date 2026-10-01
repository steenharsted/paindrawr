#' Create a new named (.rgba) element in the pdr list column
#' 
#' The function handles the rgba matrix calculation in two
#' distinctly different ways (as specified by the 'type'
#' parameter).
#' 
#' If 'type' is set to spray -- the rgba value of each pixel
#' is based on a weighted combination of spray strokes in
#' the drawing.
#' 
#' If 'type' is set to pen -- each pixel will have an rgba 
#' value of 0 or 1 indicating whether the pixel in inside or
#' outside the polygons defined by each pen stroke.
#'
#' @param pdr_list
#' @param type
#'
#' @returns
#'
#' @export
#' @examples
pdr_add_rgba <- function(
  pdr_list, 
  method = "memory",
  clean_up = TRUE,
  dpi = 96,
  rgba_only = FALSE,
  buffer=5,
  invert=FALSE,
  type="auto") {
  
  #### This function takes a pain drawing list as input and
  # adds a new .rgba element to it before returning it.
  # Depending on the input parameter 'type', the .rgba element
  # is computer by 'pdr_get_rgba_from_spray' or '..from_pen'
  
  #### Sanity checks
  if(!type %in% c("auto", "spray", "pen")) {
    warning(paste0("Parameter 'type' ", type, " unknown. Must be 'atuo', 'spray' or 'pen'"))
    return(NA)
  }

  #### Three options for 'type:
  if (type=="auto") {
    # Get set type based on '.tool' element in data
    tool <- pdr_example_data[1:2,]$pdr_list |> 
      purrr::map_dfr(\(s) {s$.strokes |> dplyr::select(.tool)}) |> 
      dplyr::pull(.tool) |> 
      unique()

    if(length(tools)>1) {
      warning(paste0("Parameter 'type' is 'auto', but multiple .tool types defined in data -- specify type as 'pen' or 'spray'."))
    return(NA)
    }
  }

  if (type == "spray") {
    # Return this 
    pdr_get_rgba_from_spray(
      pdr_list,
      method,
      clean_up,
      dpi,
      rgba_only)
  } else if (type == "pen") {
    # First ensure there is a .polygons element in data
    names_of_elements <- pdr_list |> 
      purrr::map(\(e) {names(e)})
    if(!all(".polygons" %in% names_of_elements)) {
      pdr_list <- pdr_add_polygons(pdr_list, buffer)
    }
    # Then return this
    pdr_get_rgba_from_pen(pdr_list, invert)
  }
}