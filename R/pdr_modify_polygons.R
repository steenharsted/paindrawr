#' Modify pain drawing elements
#' 
#' This function takes a valid pain drawing list as input
#' and performs one or more operations sequentially on those 
#' data before returning a valid pain drawing list-column of 
#' the same length. It is thus suited for `mutate()` operations.
#' 
#' 
#' @param .data A valid pain drawing tibble
#' @param paindrawr_data The name of the pain drawing list-col in .data
#' @param ops A string vector of operations to perform. See below:
#' 
#' Parameter 'ops' can be one of the following, which will be performed the listd order:
#' * "make_valid" Removes self-intersection and other issue hich invalidate polygons -- see `sf::st_make_valid()`
#' * "reduce_to_chull" Reduces polygons to their convex hull -- see `sf::st_convex_hull()`
#' * "merge_overlaps" Merges (union) overlapping polygons into one -- see `sf::st_union()`
#'
#' @returns A valid pain drawing tibble
#' 
#' @export 
#' 
#' @examples
#' # Plot gross anatomical regions
#' pdr_example_anatomy |>
#'   pdr_implode() |>
#'   mutate(pdr_data = pdr_polygonize(pdr_data)) |>
#'   pdr_plot_polygons(pdr_data)
#' 
#' # Plot merged anatomical regions for body outline
#' pdr_example_anatomy |>
#'   pdr_implode() |>
#'   mutate(pdr_data = pdr_polygonize(pdr_data)) |>
#'   mutate(pdr_data = pdr_modify_polygons(pdr_data, "merge_overlaps")) |>
#'   pdr_plot_polygons(pdr_data)
 

pdr_modify_polygons <- function(paindrawr_data, ops=NULL, template=NULL, polygons_only=FALSE) {
  if(!is.null(template) && length(template)!=1 && length(template)!=length(paindrawr_data)) {
    warning("Parameter 'template' should be a list of length 1 or the same as paindrawr_data")
    return(NA)
  }

  ########## Sanity checks ##########
  accepted_ops <- c("reduce_to_chull", "merge_overlaps", "make_valid", "merge_edges", "reduce_by_template")

  # This function takes a valid pain drawing data set as input
  # if (!pdr_check_data(pdr, verbose = FALSE)) {
  #   pdr_check_data(pdr, verbose = TRUE) # Give user some info
  #   stop("Stopped: The function `pdr_modify()` expects valid pain data as parameter 'pdr'. Use `pdr_check_data()` for more details.")
  # }
  # # Require 'ops' to be a character vector of fixed options
  # if(!is.character(ops)) {
  #   stop("Stopped: The function `pdr_modify()` expects a character vector as parameter 'ops'.")
  # } 
  # if(any(identical(NA, ops)) || any(ops == "")) {
  #   warning("Unknown ops specified in function `pdr_modify()`.")
  #   return(pdr)
  # }
  # if(any(identical(ops, "reduce_by_template")) && is.null(templates)) {
  #   stop("Stopped: The function `pdr_modify()` expects a templates parameter when 'ops' includes 'reduce_to_template'.")
  # }
  # if(any(!{ops %in% accepted_ops})) {
  #   warning("Unknown ops specified in function `pdr_modify()`.")
  # }
  # if(all(c("merge_overlaps", "merge_edges") %in% ops)) {
  #   warning("Both 'merge_overlaps' and 'merge_edges' specified in 'ops' of `pdr_modify()` -- defaulting to 'merge_overlaps'.")
  #   ops <- ops[-which(ops == "merge_edges")]
  # }


  ########## End sanity checks ##########

  ########## Helper functions ##########

  ########## End helper functions ########## 

  ########## ops functions ##########


  ########## End ops functions ########## 
  if("make_valid" %in% ops) {
    paindrawr_data <- paindrawr_data |>
      purrr::map_depth(.depth=1, \(pd) {
        pd <- pd |> 
          purrr::imap(\(element,indx) {
            if(indx==".polygons") {
              element |> 
                purrr::map(~sf::st_make_valid(.x)) |>
                purrr::map(~sf::st_buffer(.x, dist=0)) |>
                sf::st_sfc()
            } else {
              element
            }
          })
      })
  }

  if("reduce_to_chull" %in% ops) {
    paindrawr_data <- paindrawr_data |>
      purrr::map_depth(.depth=1, \(pd) {
        pd <- pd |> 
          purrr::imap(\(element,indx) {
            if(indx==".polygons") {
              element |> 
                purrr::map(\(poly) {sf::st_convex_hull(poly)}) |> 
                sf::st_sfc()
            } else {
              element
            }
          })
      })
  }

  if("merge_overlaps" %in% ops) {
    paindrawr_data <- paindrawr_data |>
      purrr::map(\(e) {
        e$.polygons <- e$.polygons |>
          sf::st_union() |>
          sf::st_cast("POLYGON")
        e
      })
  }

  if("reduce_by_template" %in% ops) {
    # print(str(p$.polygons))
    # print("---")
    # print(str(template))
    paindrawr_data <- purrr::map2(paindrawr_data, template, \(p,t) {
        p$.polygons <- 
          sf::st_intersection(p$.polygons, t) |>
          sf::st_cast("POLYGON")
        p
      })
  }

  if(polygons_only) {
    paindrawr_data |> purrr::map(".polygons")
  } else {
    paindrawr_data # return this
  }
}