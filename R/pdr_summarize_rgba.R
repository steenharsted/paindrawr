#' Summarise the alpha values of drawn pixels in a pain drawing
#'
#' Computes summary statistics of the alpha channel of the `.rgba` array of
#' each drawing, using only pixels whose alpha falls within `alpha_range`
#' (lower bound exclusive, upper bound inclusive). The number of drawn pixels
#' (`px`) and the proportion of the canvas covered (`area_prop`) describe how
#' much was drawn, while e.g. `mean` or `median` describe how dark or opaque
#' the strokes are. Optionally, each drawing is first multiplied by one or more
#' stencils (e.g. body regions) so that statistics are computed per region.
#'
#' The drawings must already contain `.rgba`; see [pdr_add_rgba()]. For the
#' terms pdr_drawing, pdr_list and pdr_tibble, see [pdr_match()].
#'
#' @inheritParams pdr_plot_rgba
#' @param pdr_col <[`tidy-select`][tidyselect::language]> One or more
#'   list-columns of `.data` (pdr_lists) with `.rgba` in each drawing. Defaults
#'   to `pdr_data`. Unquoted names, strings and selections such as
#'   `c(a, b)` are accepted; each selected column is summarised separately.
#' @param summary_stat Character vector of statistics to compute from the alpha
#'   values of the included pixels. Any of `"px"` (number of pixels),
#'   `"area_prop"` (`px` divided by `.width * .height`), `"mean"`, `"sd"`,
#'   `"median"`, `"iqr"`, `"min"`, `"max"`, `"q1"`, `"q5"`, `"q10"`, `"q25"`,
#'   `"q75"`, `"q90"`, `"q95"` and `"q99"` (the `q` statistics are quantiles
#'   at probabilities 0.01, 0.05, ..., 0.99). Two shortcuts are available when
#'   given as a single string: `"seven"` (default) is `px`, `mean`, `max`,
#'   `min`, `median`, `sd` and `iqr`; `"all"` is `seven` plus `area_prop`,
#'   `q1`, `q5`, `q25`, `q75`, `q90`, `q95` and `q99` (note: `q10` is *not*
#'   included in `"all"`, but can be requested explicitly). Columns are
#'   returned in a fixed order, not in the order requested: `px`, `area_prop`,
#'   `mean`, `sd`, `median`, `iqr`, `min`, `max`, `q1`, `q5`, `q10`, `q25`,
#'   `q75`, `q90`, `q95`, `q99` (restricted to those requested). Note this
#'   differs from the order in which `"seven"` is listed above.
#' @param alpha_range Numeric vector of length 2 giving the lower (exclusive)
#'   and upper (inclusive) alpha bounds of pixels to include, with
#'   `0 <= alpha_range[1] < alpha_range[2] <= 1`. Defaults to `c(0, 1)`, i.e.
#'   all drawn pixels. Use e.g. `c(0.5, 1)` to restrict to heavily drawn pixels.
#' @param stencils Optional pdr_list in which each element has a `.stencil`
#'   (a height x width matrix of 0/1) and an `.id`, e.g. the list-column
#'   `pdr_stencil_full_body$pdr_data` or a list built with [pdr_add_stencil()]. Each stencil is applied
#'   (multiplied) to the alpha channel of every drawing before summarising, so
#'   the stencil dimensions must match the drawings. Defaults to `NULL`
#'   (no stencil). Note that this is deliberately plural and differs from
#'   `stencil` in [pdr_modify_rgba()].
#'
#' @returns `.data` (a pdr_tibble) with added numeric columns, one per selected
#'   `pdr_col` and statistic (and per stencil, if `stencils` is given). Columns
#'   are named `<pdr_col>_<stat>` without stencils, and
#'   `<pdr_col>_<stencil id>_<stat>` with stencils, e.g. `pdr_data_mean` or
#'   `pdr_data_Back_head_mean`. If no pixels fall in `alpha_range`, `px` is 0,
#'   `mean`, `sd`, `median`, `iqr` and quantiles are `NA`, and `min`/`max` are
#'   `Inf`/`-Inf` (with a warning). If a drawing is missing (`NA`), its `.rgba`
#'   is `NA`, or its alpha channel is entirely `NA`, all statistics for that
#'   drawing, including `px`, are `NA`. Individual `NA` pixels are ignored.
#'
#' @seealso [pdr_add_rgba()] to add `.rgba`, [pdr_add_stencil()] and
#'   [pdr_modify_rgba()] for stencils, [pdr_plot_rgba()] to plot the arrays.
#'
#' @importFrom stats quantile sd median IQR
#' @export
#' @examples
#' pd <- pdr_example_data |>
#'   dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
#'
#' # Default: the "seven" statistics across all drawn pixels
#' pdr_summarize_rgba(pd)
#'
#' # Selected statistics, only heavily drawn pixels
#' pdr_summarize_rgba(
#'   pd,
#'   summary_stat = c("px", "area_prop", "mean"),
#'   alpha_range = c(0.5, 1)
#' )
#'
#' # Per stencil (here, two body regions)
#' pdr_summarize_rgba(
#'   pd,
#'   summary_stat = "px",
#'   stencils = pdr_stencil_full_body$pdr_data[1:2]
#' )
pdr_summarize_rgba <- function(
  .data,
  pdr_col = pdr_data,
  summary_stat = "seven",
  alpha_range = c(0, 1),
  stencils = NULL
) {
  ############################
  ##### Helper functions #####
  ############################

  # We must handle both string input, unquaoted col names, and 1 or more at the same time
  pdr_col <- names(
    tidyselect::eval_select(
      rlang::enquo(pdr_col),
      .data
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
        # A missing drawing (NA), missing .rgba, or an alpha channel that is
        # entirely NA cannot be summarised: all statistics are NA (not px = 0)
        is_missing <- !is.list(pdr_drawing) ||
          !is.array(pdr_drawing$.rgba) ||
          all(is.na(pdr_drawing$.rgba[,, 4]))

        if (is_missing) {
          alpha_channel <- numeric(0)
        } else {
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
        }

        result_tib <- tibble::tibble(.rows = 1)
        if ("px" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            px = length(alpha_channel)
          )
        }
        if ("area_prop" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            area_prop = if (is_missing) {
              NA_real_
            } else {
              length(alpha_channel) / (pdr_drawing$.width * pdr_drawing$.height)
            }
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
          result_tib <- dplyr::bind_cols(
            result_tib,
            min = if (is_missing) NA_real_ else min(alpha_channel)
          )
        }
        if ("max" %in% summary_stat) {
          result_tib <- dplyr::bind_cols(
            result_tib,
            max = if (is_missing) NA_real_ else max(alpha_channel)
          )
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
        if (is_missing) {
          result_tib[] <- NA_real_
        }
        names(result_tib) <- paste0(col_name, "_", names(result_tib))
        return(result_tib)
      })
  }

  #########################
  ##### Sanity checks #####
  #########################
  if (identical(summary_stat, "seven")) {
    summary_stat <- c("px", "mean", "max", "min", "median", "sd", "iqr")
  } else if (identical(summary_stat, "all")) {
    summary_stat <- c(
      "px",
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
  # Every non-missing drawing in each selected column has .rgba
  for (i in pdr_col) {
    if (
      !all(purrr::map_lgl(
        .data[[i]],
        \(d) !is.list(d) || ".rgba" %in% names(d)
      ))
    ) {
      stop(
        "Not all drawings in column '",
        i,
        "' contain a .rgba element; see pdr_add_rgba()"
      )
    }
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
    for (i in pdr_col) {
      for (j in seq_along(stencils)) {
        .data <- dplyr::bind_cols(
          .data,
          summarize_drawing_as_tibble(
            .data[[i]],
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
    for (i in pdr_col) {
      .data <- dplyr::bind_cols(
        .data,
        summarize_drawing_as_tibble(
          .data[[i]],
          summary_stat,
          alpha_range,
          NULL, # no stencil
          i # name
        )
      )
    }
  }

  .data
}
