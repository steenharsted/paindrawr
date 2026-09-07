#' Summarise alpha intensity of drawn pixels in a pain drawing
#'
#' Computes a summary statistic of alpha values across all pixels whose alpha
#' falls strictly within `alpha_range`. This captures how intensely a patient
#' drew -- i.e. how dark or opaque the strokes are -- independently of how much
#' area was covered. Use in combination with [pdr_summarize_alpha_area()] to
#' distinguish between a small intense drawing and a large light drawing.
#'
#' @param rgbas Either a list of RGBA arrays (each `[height, width, 4]`), or a
#'   list of paindrawing data objects containing a `.rgba` element (as produced
#'   by [pdr_add_rgba()]). Typically passed as a list-column inside
#'   `dplyr::mutate()`.
#' @param summary_stat A string specifying the summary statistic to compute
#'   over drawn pixel alpha values. One of `"mean"` (default), `"max"`,
#'   `"min"`, `"median"`, or `"sd"`.
#' @param alpha_range A numeric vector of length 2 specifying the lower
#'   (exclusive) and upper (inclusive) bounds of the alpha range to include.
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels. Use e.g. `c(0.5, 1)` to
#'   restrict to heavily drawn pixels only.
#'
#' @returns A numeric vector the same length as `rgbas`. Each element is the
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
#'   dplyr::mutate(rgba = pdr_add_rgba(pdr_data))
#'
#' # Mean intensity across all drawn pixels (using rgba list-column)
#' pdr_summarize_alpha_intensity(pd$rgba)
#'
#' # Max intensity, passed pdr_data directly
#' pdr_summarize_alpha_intensity(pd$pdr_data, summary_stat = "max")
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(
#'   intensity_mean = pdr_summarize_alpha_intensity(pdr_data),
#'   intensity_max  = pdr_summarize_alpha_intensity(pdr_data, summary_stat = "max")
#' )
pdr_summarize_alpha_intensity <- function(
  rgbas,
  summary_stat = c("mean", "max", "min", "median", "sd"),
  alpha_range = c(0, 1)
) {
  # Wrap rgba in list if it provided as a single raw
  if (!is.list(rgbas)) {
    rgbas <- list(rgbas)
  }

  # If given paindrawing data, extract .rgba
  if (!is.array(rgbas[[1]])) {
    if (is.null(rgbas[[1]][[".rgba"]])) {
      stop("No `.rgba` found. Run `pdr_add_rgba()` first.")
    }
    rgbas <- purrr::map(rgbas, ".rgba")
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

  summary_stat <- match.arg(summary_stat)

  # --- get summary stat ---

  purrr::map_dbl(rgbas, function(rgba) {
    if (!is.array(rgba) || length(dim(rgba)) != 3 || dim(rgba)[3] < 4) {
      stop(
        "`rgbas` must be a list of numeric arrays with dimensions [height, width, 4]."
      )
    }
    a <- as.double(rgba[,, 4])
    drawn <- a[a > alpha_range[1] & a <= alpha_range[2]]
    if (length(drawn) == 0) NA_real_ else do.call(summary_stat, list(drawn))
  })
}


#' Calculate the proportion of canvas covered in a pain drawing
#'
#' Computes the proportion of total pixels whose alpha value falls strictly
#' within `alpha_range`. This captures how much of the canvas was drawn on,
#' independently of stroke intensity. Use in combination with
#' [pdr_summarize_alpha_intensity()] to distinguish between a small intense drawing
#' and a large light drawing.
#'
#' @param rgbas Either a list of RGBA arrays (each `[height, width, 4]`), or a
#'   list of paindrawing data objects containing a `.rgba` element (as produced
#'   by [pdr_add_rgba()]). Typically passed as a list-column inside
#'   `dplyr::mutate()`.
#' @param alpha_range A numeric vector of length 2 specifying the lower
#'   (exclusive) and upper (inclusive) bounds of the alpha range to include.
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels. Use e.g. `c(0.5, 1)` to
#'   restrict to heavily drawn pixels only.
#'
#' @returns A numeric vector the same length as `rgbas`. Each element is the
#'   proportion of canvas pixels with alpha in `(alpha_range[1], alpha_range[2]]`
#'   for the corresponding drawing, i.e. a value between 0 and 1.
#'
#' @seealso [pdr_summarize_alpha_intensity()] for the complementary intensity-based
#'   metric, [pdr_add_rgba()] to add `.rgba` to paindrawing data.
#'
#' @export
#' @examples
#' pd <- pdr_import_json("data-raw/two_geoms.json") |>
#'   dplyr::mutate(rgba = pdr_add_rgba(pdr_data))
#'
#' # Proportion of all drawn pixels (using rgba list-column)
#' pdr_summarize_alpha_area(pd$rgba)
#'
#' # Passed pdr_data directly
#' pdr_summarize_alpha_area(pd$pdr_data)
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(
#'   area         = pdr_summarize_alpha_area(pdr_data),
#'   area_over_50 = pdr_summarize_alpha_area(pdr_data, alpha_range = c(0.5, 1))
#' )
pdr_summarize_alpha_area <- function(rgbas, alpha_range = c(0, 1)) {
  # Wrap rgba in list if it provided as a single raw
  if (!is.list(rgbas)) {
    rgbas <- list(rgbas)
  }

  # If given paindrawing data, extract .rgba
  if (!is.array(rgbas[[1]])) {
    if (is.null(rgbas[[1]][[".rgba"]])) {
      stop("No `.rgba` found. Run `pdr_add_rgba()` first.")
    }
    rgbas <- purrr::map(rgbas, ".rgba")
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

  purrr::map_dbl(rgbas, function(rgba) {
    if (!is.array(rgba) || length(dim(rgba)) != 3 || dim(rgba)[3] < 4) {
      stop(
        "`rgbas` must be a list of numeric arrays with dimensions [height, width, 4]."
      )
    }
    a <- as.double(rgba[,, 4])
    sum(a > alpha_range[1] & a <= alpha_range[2]) / length(a)
  })
}
