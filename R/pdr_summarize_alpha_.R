#' Summarise alpha intensity of drawn pixels in a pain drawing
#'
#' Computes a summary statistic of alpha values across all pixels whose alpha
#' falls strictly within `alpha_range`. This captures how intensely a patient
#' drew -- i.e. how dark or opaque the strokes are -- independently of how much
#' area was covered. Use in combination with [pdr_summarize_alpha_area()] to
#' distinguish between a small intense drawing and a large light drawing.
#'
#' @param pdr_list Either a list of RGBA arrays (each `[height, width, 4]`), or a
#'   list of paindrawing data objects containing a `.rgba` element (as produced
#'   by [pdr_add_rgba()]). Typically passed as a list-column inside
#'   `dplyr::mutate()`.
#' @param summary_stat A string specifying the summary statistic to compute
#'   over drawn pixel alpha values. One of `"mean"` (default), `"max"`,
#'   `"min"`, `"median"`, `"sd"`, `"raw"` or `"seven"`. If `"seven"` the function 
#'   returns a list column 5 summary stats. If `"raw"` the function returns
#'   a list column of numeric vectors will all resulting alpha values.
#' @param alpha_range A numeric vector of length 2 specifying the lower
#'   (exclusive) and upper (inclusive) bounds of the alpha range to include.
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels. Use e.g. `c(0.5, 1)` to
#'   restrict to heavily drawn pixels only.
#' @param stencils A list (length == 1 or same length as pdr_list) of (2-dimensional)
#'   matrices of the same x and y dimension as pdr_list Expected values are 0
#'   and 1 -- if provided, the stencils are applied (multiplied) with the
#'   rgba arrays before summarising.
#'
#' @returns A numeric vector the same length as `pdr_list`. Each element is the
#'   requested summary statistic of alpha values for pixels within
#'   `(alpha_range[1], alpha_range[2]]`, or `NA_real_` if no pixels fall in
#'   that range.
#'
#' @seealso [pdr_summarize_alpha_area()] for the complementary area-based metric,
#'   [pdr_add_rgba()] to add `.rgba` to paindrawing data.
#'
#' @export
#' @examples
#' pd <- pdr_import_json("data-raw/two_geoms.json") |>
#'   dplyr::mutate(rgba = pdr_add_rgba(pdr_list))
#'
#' # Mean intensity across all drawn pixels (using rgba list-column)
#' pdr_summarize_alpha_intensity(pd$rgba)
#'
#' # Max intensity, passed pdr_list directly
#' pdr_summarize_alpha_intensity(pd$pdr_list, summary_stat = "max")
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(
#'   intensity_mean = pdr_summarize_alpha_intensity(pdr_list),
#'   intensity_max  = pdr_summarize_alpha_intensity(pdr_list, summary_stat = "max")
#' )
pdr_summarize_alpha_intensity <- function(
  pdr_tibble,
  pdr_list_cols = pdr_data, 
  summary_stat = c("mean", "max", "min", "median", "sd", "iqr", "all"),
  alpha_range = c(0, 1),
  stencils = NULL
) {
  ############################
  ##### Helper functions #####
  ############################

  get_summary_as_tibble <- function(one_pdr_list, summary_stat, alpha_range, one_stencil, col_name) {
    # This function is intended to take a single pdr_list as input and iterate over each drawing
    # Apply a single stencil (a matrix) and apply and alpha range, before calculating a number
    # of summary statistics and retuning the result as a tibble of same number of rows as pdr_list
    # with columns of col_name as prefix to the statistical summary function.

    one_pdr_list |> purrr::map_dfr(\(pdr_drawing) {
      # apply stencil while alpha_channel dimensions are unchanged
      alpha_channel <- pdr_drawing$.rgba[,,4]
      if(all(!is.na(one_stencil) & !is.null(one_stencil))) {
        alpha_channel <- alpha_channel * one_stencil  
      }
      # apply alpha range -- WARNING alpha_channel is now no longer an array
      alpha_channel <- alpha_channel[!is.na(alpha_channel)]
      alpha_channel <- alpha_channel[alpha_channel > alpha_range[1] & alpha_channel <= alpha_range[2]]
      
      sapply(summary_stat, \(f) {match.fun(f)(alpha_channel)}) |>
        dplyr::bind_rows() |>
        dplyr::rename_with(~paste0(col_name,.x, sep="_"), everything())# map return
    })
  }


  #########################
  ##### Sanity checks #####
  #########################
  # adapt the summary_stat to suit function names
  summary_stat <- stringr::str_replace_all(summary_stat, "iqr", "IQR") |> unique()
  if(summary_stat=="all") {summary_stat <- c("mean", "max", "min", "median", "sd", "IQR")}
  # is pdr_tibble actually a pdr_tibble?
  if(!pdr_match(pdr_tibble, as="pdr_tibble")) {stop("Invalid 'pdr_tibble' parameter")}
  # are all of pdr_list valid col names in the pdr_tibble
  if(!all(pdr_list_cols %in% names(pdr_tibble))) {stop("Invalid 'pdr_list_cols' parameter - not all valid col name")}
  # are all specified pdr_list columns actually a pdr_list?
  if(!all(
    pdr_tibble |> 
      dplyr::select( {{ pdr_list_cols }}) |>
      dplyr::summarize_all(~pdr_match(.x, as="pdr_list"))
  )) {stop("Invalid 'pdr_list parameter - not all valid pdr_list.")}
  # Check .rgba exists in all
  tmp <- pdr_tibble |> dplyr::select({{pdr_list_cols}}) |> 
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(col) purrr::map_lgl(col, \(x) ".rgba" %in% names(x)) |> all()
      )
    )
  if(!all(tmp)) {stop("Parameter 'pdr_list' is invalid as not all elements contain .rgba data")}
  
  # alpha_range is valid
  if (
    !is.numeric(alpha_range) ||
      length(alpha_range) != 2 ||
      any(is.na(alpha_range)) ||
      alpha_range[1] < 0 ||
      alpha_range[2] > 1 ||
      alpha_range[1] >= alpha_range[2]
  ) {
    stop(
      "`alpha_range` must be a numeric vector of length 2 with 0 <= alpha_range[1] < alpha_range[2] <= 1."
    )
  }
  # Check stencils is a pdr_list
  if(!pdr_match(stencils, as="pdr_list")) {
    if(!is.null(stencils)) {stop("Paramter 'stencils' is not a valid pdr_list with .stencil elements")}
  }
  # summary_stat ok
  summary_stat <- match.arg(summary_stat)
  #########################
  ### End sanity cheks ####
  #########################
  
  ##########################
  ####### Main loops #######
  ##########################
  
  # Loop pdr_list_cols
    # loop stencil
      # map prd_list
  for(i in pdr_list_cols) {
    for(j in seq_along(stencils)) {
      pdr_tibble <- dplyr::bind_cols(
        pdr_tibble,
        get_summary_as_tibble(
          pdr_tibble[[i]], 
          summary_stat,
          alpha_range,
          stencils[[j]]$.stencil, # one stencil
          paste0(i,stencils[[j]]$.id,sep="_") # name
        )
      )
    }
  }


}


#' Calculate the proportion of canvas covered in a pain drawing
#'
#' Computes the proportion of total pixels whose alpha value falls strictly
#' within `alpha_range`. This captures how much of the canvas was drawn on,
#' independently of stroke intensity. Use in combination with
#' [pdr_summarize_alpha_intensity()] to distinguish between a small intense drawing
#' and a large light drawing.
#'
#' @param pdr_list Either a list of RGBA arrays (each `[height, width, 4]`), or a
#'   list of paindrawing data objects containing a `.rgba` element (as produced
#'   by [pdr_add_rgba()]). Typically passed as a list-column inside
#'   `dplyr::mutate()`.
#' @param alpha_range A numeric vector of length 2 specifying the lower
#'   (exclusive) and upper (inclusive) bounds of the alpha range to include.
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels. Use e.g. `c(0.5, 1)` to
#'   restrict to heavily drawn pixels only.
#'
#' @returns A numeric vector the same length as `pdr_list`. Each element is the
#'   proportion of canvas pixels with alpha in `(alpha_range[1], alpha_range[2]]`
#'   for the corresponding drawing, i.e. a value between 0 and 1.
#'
#' @seealso [pdr_summarize_alpha_intensity()] for the complementary intensity-based
#'   metric, [pdr_add_rgba()] to add `.rgba` to paindrawing data.
#'
#' @export
#' @examples
#' pd <- pdr_import_json("data-raw/two_geoms.json") |>
#'   dplyr::mutate(rgba = pdr_add_rgba(pdr_list))
#'
#' # Proportion of all drawn pixels (using rgba list-column)
#' pdr_summarize_alpha_area(pd$rgba)
#'
#' # Passed pdr_list directly
#' pdr_summarize_alpha_area(pd$pdr_list)
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(
#'   area         = pdr_summarize_alpha_area(pdr_list),
#'   area_over_50 = pdr_summarize_alpha_area(pdr_list, alpha_range = c(0.5, 1))
#' )
pdr_summarize_alpha_area <- function(pdr_list, alpha_range = c(0, 1)) {
  # Wrap rgba in list if it provided as a single raw
  if (!is.list(pdr_list)) {
    pdr_list <- list(pdr_list)
  }

  # If given paindrawing data, extract .rgba
  if (!is.array(pdr_list[[1]])) {
    if (is.null(pdr_list[[1]][[".rgba"]])) {
      stop("No `.rgba` found. Run `pdr_add_rgba()` first.")
    }
    pdr_list <- purrr::map(pdr_list, ".rgba")
  }

  # --- input checks ---
  if (
    !is.numeric(alpha_range) ||
      length(alpha_range) != 2 ||
      any(is.na(alpha_range)) ||
      alpha_range[1] < 0 ||
      alpha_range[2] > 1 ||
      alpha_range[1] >= alpha_range[2]
  ) {
    stop(
      "`alpha_range` must be a numeric vector of length 2 with 0 <= alpha_range[1] < alpha_range[2] <= 1."
    )
  }

  unname(purrr::map_dbl(pdr_list, function(rgba) {
    if (!is.array(rgba) || length(dim(rgba)) != 3 || dim(rgba)[3] < 4) {
      stop(
        "`pdr_list` must be a list of numeric arrays with dimensions [height, width, 4]."
      )
    }
    a <- as.double(rgba[,, 4])
    sum(a > alpha_range[1] & a <= alpha_range[2]) / length(a)
  }))
}
