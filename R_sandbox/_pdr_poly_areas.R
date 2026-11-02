#' Calculate area of markings/strokes for pain drawings
#'
#' Calculate the areas either on a per-stroke basis, or a cummulated per-drawing basis.
#' 
#' @param pdr A valid pain drawing data list-col (see @pdr_check_data())
#' @param by A string of either 'strokes', 'drawings' or 'both'. 
#'
#' @returns If `by` is "strokes", the function will return a list of tibbles of stroke areas (as integers) -- one list element (tibble) for each row in `pdr`. 
#' If `by` is "drawings", the function returns a vector of collated drawing areas (as integers) of the same length as `pdr`. 
#'
#' @export
#' @examples
#' tibble::tibble(pdr_data = pdr_example_anatomy) |>
#'   dplyr::mutate(region = pdr_get_info(pdr_data, ".id")) |>
#'   dplyr::filter(stringr::str_detect(region, "Back_right")) |>
#'   dplyr::mutate(cum_area = pdr_poly_areas(pdr_data, by="drawings"))
#'
#' tibble::tibble(pdr_data = pdr_example_data[1:3]) |>
#'   dplyr::mutate(region = pdr_get_info(pdr_data, ".id")) |>
#'   dplyr::mutate(stroke_areas = pdr_poly_areas(pdr_data, by="strokes"))
#' 
#'  
pdr_poly_areas <- function(paindrawr_data = pdr_data, by="drawings") {
    
   tmp <- paindrawr_data |> 
     purrr::map(~sf::st_area(.x$.polygons))
  
  if (by=="strokes") {
    tmp # return
  } else {
    tmp |>
      purrr::map_dbl(\(e) {sum(e)})      
  }
}