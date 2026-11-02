#' Modify the rgba data of pain drawings
#'
#' Modifies the `.rgba` array of each pdr_drawing in a pdr_list, e.g. to apply
#' a stencil that restricts a drawing to an anatomical region, or to trim
#' low/high alpha values. It returns a pdr_list of the same length and is thus
#' suited for use inside `mutate()`. The drawings must already have an `.rgba`
#' element (see [pdr_add_rgba()]). See [pdr_match()] for the definitions of
#' pdr_list and pdr_drawing.
#'
#' @param pdr_list A pdr_list (see [pdr_match()]) whose pdr_drawings contain
#'   `.rgba`, e.g. the `pdr_data` column of a pdr_tibble.
#' @param op A string specifying the operation. There is no default (the
#'   default `""` raises an error); one of:
#'   * `"apply_stencil"` — multiplies the alpha channel (4th layer) of
#'     `.rgba` by `stencil`, so that pixels outside the stencil become fully
#'     transparent. Requires `stencil`.
#'   * `"multiply"` — multiplies the whole `.rgba` array by `stencil`.
#'     Requires `stencil`.
#'   * `"addition"` — adds `stencil` to the whole `.rgba` array. Requires
#'     `stencil`.
#'   * `"low_trim"` — sets all `.rgba` values below `threshold` to 0.
#'   * `"high_trim"` — sets all `.rgba` values above `threshold` to 1.
#' @param stencil The stencil(s) used by the ops `"apply_stencil"`,
#'   `"multiply"` and `"addition"`; otherwise ignored. Either
#'   * a single matrix,
#'   * a list of matrices, or
#'   * a pdr_list in which every pdr_drawing has a `.stencil` element (see
#'     [pdr_add_stencil()]), e.g. `pdr_stencil_full_body$pdr_data`.
#'
#'   The length must be 1 (the stencil is used for every drawing) or equal to
#'   the length of `pdr_list` (stencils are paired with drawings by position).
#'   The height and width of each stencil must match the `.rgba` of the
#'   drawing it is paired with. Default is `NULL`.
#' @param threshold A number between 0 and 1 used by `"low_trim"` and
#'   `"high_trim"`; otherwise ignored. Default is `0.5`.
#'
#' @returns A pdr_list of the same length as `pdr_list`, with modified `.rgba`
#'   elements.
#'
#' @seealso [pdr_add_stencil()], [pdr_add_rgba()], [pdr_plot_stencil()]
#'
#' @export
#' @examples
#' # Keep only the pain drawn within the first region of the full body stencils
#' pdr_example_data[1:2, ] |>
#'   dplyr::mutate(
#'     pdr_data = pdr_add_rgba(pdr_data) |>
#'       pdr_modify_rgba(
#'         op = "apply_stencil",
#'         stencil = pdr_stencil_full_body$pdr_data[1]
#'       )
#'   ) |>
#'   pdr_plot_rgba()
#'
#' # Remove faint (low alpha) pixels
#' pdr_example_data[1:2, ] |>
#'   dplyr::mutate(
#'     pdr_data = pdr_add_rgba(pdr_data) |>
#'       pdr_modify_rgba(op = "low_trim", threshold = 0.3)
#'   )
pdr_modify_rgba <- function(pdr_list, op="", stencil=NULL, threshold = 0.5) {
  # Sanity checks -- see stop message for explanation
  if(!op %in% c("multiply", "addition", "apply_stencil", "low_trim","high_trim")) { stop("Parameter 'op' has invalid value.")}
  if(!is.numeric(threshold) | threshold<0 | threshold>1) {stop("Parameter 'threshold' has invalid value.")}
  pdr_list <- pdr_match(pdr_list, action="coerce", as="pdr_list")

  

  #### Now coerce the stencil parameter to a list of matrices:
  # stencil can be a pdr_list with a .stencil element, or
  # stencil can be a single matrix, or a list of matrices 
  if(!is.null(stencil)){
    if(is.matrix(stencil)) { 
      # stencil is a matrix so:
      # make stencil a list of length 1 (unnamed) with a matrix
      stencil <- list(stencil)
    } else if(is.list(stencil) && all(stencil |> purrr::map_lgl(\(e_stencil) {is.matrix(e_stencil)}))) {
      # stencil is a list of matrices so:
      # leave stencil as is
    } else if(pdr_match(stencil, action="eval", as="pdr_list") &&
              all(stencil |> purrr::map_lgl(\(e_stencil) {".stencil" %in% names(e_stencil)}))) {
      # stencil is a list of lists with named elements '.stencil. so:
      # make stencil a list of the .rgba matrices
      stencil <- stencil |> purrr::map(".stencil")
    } else {
      stop("Parameter 'stencil' must be a) a matrix, b) a list of matrices or c) pain drawing list column with .stencil elements.")
    }
  
    # Further sanity checks  -- see stop message for explanation
    if(length(stencil)!=1 && length(stencil)!=length(pdr_list)) {
      stop("Parameter stencil must be coercible to a list of matrices of length 1 or same length as parameter pdr_list")
    } 
    if(!all(purrr::map2_lgl(pdr_list, stencil, \(e_pl, e_stencil) {identical(dim(e_pl$.rgba)[1:2], dim(e_stencil)[1:2])}))) {
      stop("The x and y dimensions of .rgba array from pdr_list and the stencil do not match pairwise.")
    }
  }
  
  #### This function can modify the .rgba data stored in
  # the pain drawing list column, e.g. by multiplying or
  # adding rgba matrices of the same dimensions -- this is
  # useful for a) applying templates (multiplying with matrix
  # of 0 and 1 values) and b) creating weighted heat maps 
  # (addition of 0-1 numeric value matrix).
  #
  # As pdr_list is a list of length n, the stencil parameter must
  # be either a list of length n of matrices or a single matrix
  
  if(op=="multiply") {
    purrr::map2(pdr_list, stencil, \(e_pl, e_stencil) {
      e_pl$.rgba <- e_pl$.rgba * e_stencil
    })
  } else if(op=="addition") {
    #################################################
    #### NOTE THIS MAY REQUIRE SPECIAL WEIGHTING ####
    ####            ASK STEEN ...                ####
    #################################################
    purrr::map2(pdr_list, stencil, \(e_pl, e_stencil) {
      e_pl$.rgba <- e_pl$.rgba + e_stencil
    })
  } else if(op=="low_trim") {
      pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
        pdr_drawing$.rgba <-  ifelse(pdr_drawing$.rgba<threshold,0,pdr_drawing$.rgba)
        pdr_drawing
      })
  } else if(op=="high_trim") {
      pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
        pdr_drawing$.rgba <-  ifelse(pdr_drawing$.rgba>threshold,1,pdr_drawing$.rgba)
        pdr_drawing
      })
  } else if(op=="apply_stencil") {
    pdr_list <- purrr::map2(pdr_list, stencil, \(pd, st) {
      pd$.rgba[,,4] <- pd$.rgba[,,4] * st
      pd
    })
  }
  return(pdr_list)
}
