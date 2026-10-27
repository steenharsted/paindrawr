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
#'   over drawn pixel alpha values. One or more of `"area"`, `"area_prop"`, `"mean"`,
#'   `"sd"`, `"max"`,`"min"`, `"median"`, `"q1"`, `"q5"`, `"q10"`, `"q25"`,
#'   `"q75"`, `"q90"`, `"q95"`, `"q99"`. Alternatively: `"all"`, or `seven"` (default),
#'   to return all the above, or the first seven of the above.
#' @param alpha_range A numeric vector of length 2 specifying the lower
#'   (exclusive) and upper (inclusive) bounds of the alpha range to include.
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels. Use e.g. `c(0.5, 1)` to
#'   restrict to heavily drawn pixels only.
#' @param stencils A list (length == 1 or same length as pdr_list) of (2-dimensional)
#'   matrices of the same x and y dimension as pdr_list Expected values are 0
#'   and 1 -- if provided, the stencils are applied (multiplied) with the
#'   rgba arrays before summarising.
#'
#' @returns The input pdr_tibble with a variable number of added columns.
#'   A column is added for each input pdr_list by summary stat by stencils,
#'   appropriately named. Each column is the
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
#' pdr_summarize_rgba(pd$rgba)
#'
#' # Max intensity, passed pdr_list directly
#' pdr_summarize_rgba(pd$pdr_list, summary_stat = "max")
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(
#'   intensity_mean = pdr_summarize_rgba(pdr_list),
#'   intensity_max  = pdr_summarize_rgba(pdr_list, summary_stat = "max")
#' )
pdr_summarize_rgba <- function(
  pdr_tibble,
  pdr_list = pdr_data,
  summary_stat = "seven",
  alpha_range = c(0, 1),
  stencils = NULL
) {
  ############################
  ##### Helper functions #####
  ############################

  # We must handle both string input, unquaoted col names, and 1 or more at the same time
  pdr_list <- names(
    tidyselect::eval_select(
      rlang::enquo(pdr_list),
      pdr_tibble
    )
  )

  summarize_drawing_as_tibble <- function(
    one_pdr_list,
    summary_stat,
    alpha_range,
    one_stencil,
    col_name
  ) {
    # This function is intended to take a single pdr_list as input and iterate over each drawing
    # Apply a single stencil (a matrix) and apply an alpha range, before calculating a number
    # of summary statistics and retuning the result as a tibble of same number of rows as pdr_list
    # with columns of col_name as prefix to the statistical summary function.

    # It is worth noting: The calculations are performed on the alpha channel only. Initially, we
    # apply the stencil to the full (width x height) alpha channel (.rgba[,,4]) which will retain
    # the alpha value of all pixels inside the stencil and set every the alpha value to 0 for all
    # pixels outside the stencil. We then simply filter out all pixels with alpha values not within
    # the specificed alpha_range and in the process, convert the alpha_channel from a width x height
    # matrix to a variable length vector.

    one_pdr_list |>
      purrr::map_dfr(\(pdr_drawing) {
        # apply stencil while alpha_channel dimensions are unchanged width x height
        alpha_channel <- pdr_drawing$.rgba[,, 4]
        if (!is.null(one_stencil)) {
          alpha_channel <- alpha_channel * one_stencil
        }
        # apply alpha range -- Note after this alpha_channel is no longer an array
        alpha_channel <- alpha_channel[!is.na(alpha_channel)]
        alpha_channel <- alpha_channel[
          alpha_channel > alpha_range[1] & alpha_channel <= alpha_range[2]
        ]

        result_tib <- tibble::tibble(.rows = 1)
        if ("area" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            area = length(alpha_channel)
          )
        }
        if ("area_prop" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            area_prop = length(alpha_channel) /
              (pdr_drawing$.width * pdr_drawing$.height)
          )
        }
        if ("mean" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(result_tib, mean = mean(alpha_channel))
        }
        if ("sd" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(result_tib, sd = sd(alpha_channel))
        }
        if ("median" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            median = median(alpha_channel)
          )
        }
        if ("iqr" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(result_tib, iqr = IQR(alpha_channel))
        }
        if ("min" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(result_tib, min = min(alpha_channel))
        }
        if ("max" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(result_tib, max = max(alpha_channel))
        }
        if ("q1" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q1 = quantile(alpha_channel, probs = 0.01)
          )
        }
        if ("q5" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q5 = quantile(alpha_channel, probs = 0.05)
          )
        }
        if ("q10" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q10 = quantile(alpha_channel, probs = 0.1)
          )
        }
        if ("q25" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q25 = quantile(alpha_channel, probs = 0.25)
          )
        }
        if ("q75" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q75 = quantile(alpha_channel, probs = 0.75)
          )
        }
        if ("q90" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q90 = quantile(alpha_channel, probs = 0.90)
          )
        }
        if ("q95" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q95 = quantile(alpha_channel, probs = 0.95)
          )
        }
        if ("q99" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            q99 = quantile(alpha_channel, probs = 0.99)
          )
        }
        names(result_tib) <- paste0(col_name, "_", names(result_tib))
        return(result_tib)
      })
  }

  #########################
  ##### Sanity checks #####
  #########################
  if (identical(summary_stat, "seven")) {
    summary_stat <- c("area", "mean", "max", "min", "median", "sd", "iqr")
  } else if (identical(summary_stat, "all")) {
    summary_stat <- c(
      "area",
      "area_prop",
      "mean",
      "max",
      "min",
      "median",
      "sd",
      "iqr",
      "q1",
      "q5",
      "q25",
      "q75",
      "q90",
      "q95",
      "q99"
    )
  }

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
  if (!pdr_match(stencils, as = "pdr_list")) {
    if (!is.null(stencils)) {
      stop(
        "Parameter 'stencils' is not a valid pdr_list with .stencil elements"
      )
    }
  }
  # summary_stat ok
  #summary_stat <- match.arg(summary_stat)
  #########################
  ### End sanity cheks ####
  #########################

  ##########################
  ####### Main loops #######
  ##########################

  # If stencils are supplied:
  if (!is.null(stencils)) {
    # Loop pdr_list
    # loop stencil
    # map prd_list
    for (i in pdr_list) {
      for (j in seq_along(stencils)) {
        pdr_tibble <- dplyr::bind_cols(
          pdr_tibble,
          summarize_drawing_as_tibble(
            pdr_tibble[[i]],
            summary_stat,
            alpha_range,
            stencils[[j]]$.stencil, # one stencil
            paste0(i, "_", stencils[[j]]$.id) # name
          )
        )
      }
    }
  } else {
    # stencils is NULL
    # Loop pdr_list
    # map prd_list
    for (i in pdr_list) {
      pdr_tibble <- dplyr::bind_cols(
        pdr_tibble,
        summarize_drawing_as_tibble(
          pdr_tibble[[i]],
          summary_stat,
          alpha_range,
          NULL, # no stencil
          i # name
        )
      )
    }
  }

  pdr_tibble
}
