# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Stratify Data by Creating Group Variables
#' @noRd
stratify_data <- function(.data, variables, n_groups, label_format) {
  result <- .data

  for (i in seq_along(variables)) {
    var_name <- variables[i]
    n_grp <- n_groups[i]
    grp_col_name <- paste0(".VAR", i, "_grp")

    # Extract column and check for NAs
    var_data <- result[[var_name]]
    na_count <- sum(is.na(var_data))

    if (na_count > 0) {
      cli::cli_inform(c(
        "i" = "{na_count} NA value{?s} removed from {.var {var_name}}."
      ))
      result <- result |> dplyr::filter(!is.na(!!rlang::sym(var_name)))
      var_data <- result[[var_name]]
    }

    # Detect variable type
    var_type <- vctrs::vec_ptype_abbr(var_data)

    if (var_type %in% c("fct", "chr", "ord")) {
      # Categorical variable
      levels_present <- length(unique(var_data))
      n_grp <- levels_present

      # Use as factor
      result[[grp_col_name]] <- factor(var_data)
    } else {
      # Numeric variable - use pdr_quantile for binning
      result[[grp_col_name]] <- pdr_quantile(
        var_data,
        n_grp,
        txt = label_format
      )
      result[[grp_col_name]] <- factor(result[[grp_col_name]])
      cli::cli_inform(c(
        "i" = "{.var {var_name}} binned into {n_grp} quantile groups."
      ))
    }
  }

  return(result)
}


#' Balance Group Sizes by Sampling
#' @noRd
balance_groups <- function(.data, group_vars, max_n) {
  # Calculate minimum group size across all combinations
  min_group <- .data |>
    dplyr::summarize(n = dplyr::n(), .by = dplyr::all_of(group_vars)) |>
    dplyr::pull(n) |>
    min()

  # Target n is minimum of smallest group and max_n
  target_n <- min(min_group, max_n)

  cli::cli_inform(c(
    "i" = "Smallest group has n = {min_group}.",
    "i" = "Sampling n = {target_n} per group (max_n = {max_n})."
  ))

  # Warn if reducing groups
  if (target_n < min_group) {
    cli::cli_warn(c(
      "Rows removed to balance groups (n = {target_n} per group).",
      "i" = "Strip labels show ranges from the full dataset, not the reduced sample."
    ))
  }

  # Sample equal n from each group combination
  result <- .data |>
    dplyr::slice_sample(n = target_n, by = dplyr::all_of(group_vars))

  return(result)
}


#' Recreate Spray Effect by Expanding Spray Points
#' @noRd
recreate_spray <- function(.data) {
  .data |>
    # Create spray points
    dplyr::mutate(
      spray_id = dplyr::row_number(),
      count = .point_density
    ) |>
    tidyr::uncount(count) |>
    dplyr::mutate(
      # Add random offset within spray radius
      angle = stats::runif(dplyr::n(), 0, 2 * pi),
      distance = stats::runif(dplyr::n(), 0, .spray_radius),
      .x = .x + distance * cos(angle),
      .y = .y + distance * sin(angle)
    ) |>
    dplyr::select(-spray_id, -angle, -distance)
}


#' Calculate Pain Density for Each Grid Cell and Group
#'
#' Every point is kept (one row per point), so densely drawn cells get many
#' overlapping points in the plot.
#'
#' @param .data Unnested coordinate data, one row per point.
#' @param group_totals Tibble with one row per group and the number of
#'   drawings in `n_group`. Counted before unnesting so that drawings with no
#'   strokes still count towards the group size.
#' @param group_vars Names of the group columns.
#' @param grid_size Bin width in pixels.
#' @noRd
calculate_pain_density <- function(.data, group_totals, group_vars, grid_size) {
  # Bin coordinates into grid
  data_binned <- .data |>
    dplyr::mutate(
      x_bin = round(.x / grid_size) * grid_size,
      y_bin = round(.y / grid_size) * grid_size
    )

  # Calculate density per location per group
  density <- data_binned |>
    dplyr::mutate(
      n_id_at_location = dplyr::n_distinct(.id),
      .by = c(dplyr::all_of(group_vars), x_bin, y_bin)
    ) |>
    dplyr::left_join(group_totals, by = group_vars) |>
    dplyr::mutate(
      pct = n_id_at_location / n_group
    )

  return(density)
}


#### Scaling function
# maps bw to size in mm using scale_bw funtion
pdr_scale_bw <- function(bw) {
  min_bw <- 1
  max_bw <- 20
  min_target <- 0.1
  max_target <- 5
  ((bw - min_bw) / (max_bw - min_bw)) * (max_target - min_target) + min_target
}


#' Read and validate a background image
#'
#' Shared by the plotting functions that accept a `background_image` argument.
#'
#' @param background_image `NULL`, a file path to a PNG image, or a numeric
#'   raster array as returned by [png::readPNG()].
#' @param call The environment reported in error messages. Defaults to the
#'   calling function, so errors point at e.g. `pdr_plot_heatmap()` rather
#'   than this helper.
#' @return `NULL` if no background was given, otherwise a numeric raster array.
#' @noRd
read_background_image <- function(
  background_image,
  call = rlang::caller_env()
) {
  if (is.null(background_image)) {
    return(NULL)
  }
  if (is.character(background_image)) {
    if (!file.exists(background_image)) {
      cli::cli_abort(
        "Background image file does not exist: {.path {background_image}}",
        call = call
      )
    }
    if (!grepl("\\.png$", background_image, ignore.case = TRUE)) {
      cli::cli_abort(
        "{.arg background_image} must be a PNG file (ending in {.val .png}).",
        call = call
      )
    }
    return(png::readPNG(background_image))
  }
  if (!is.numeric(background_image) || !is.array(background_image)) {
    cli::cli_abort(
      c(
        "{.arg background_image} must be a PNG file path or a raster array.",
        "x" = "You supplied {.obj_type_friendly {background_image}}.",
        "i" = "Read images with {.fn png::readPNG}."
      ),
      call = call
    )
  }
  background_image
}


#' Validate and complete an alpha_curve argument
#'
#' @param alpha_curve Numeric of length 1 or 2: `c(steepness, midpoint)`.
#' @param call The environment reported in error messages.
#' @return Numeric of length 2, `c(steepness, midpoint)`. If only a steepness
#'   is given, the midpoint defaults to 0.25, or 0.5 when steepness is 1 so
#'   that `alpha_curve = 1` gives a linear scale.
#' @noRd
check_alpha_curve <- function(alpha_curve, call = rlang::caller_env()) {
  if (
    !is.numeric(alpha_curve) ||
      !length(alpha_curve) %in% 1:2 ||
      anyNA(alpha_curve)
  ) {
    cli::cli_abort(
      "{.arg alpha_curve} must be a numeric vector of length 1 or 2: {.code c(steepness, midpoint)}.",
      call = call
    )
  }
  if (alpha_curve[[1]] <= 0) {
    cli::cli_abort(
      c(
        "The steepness in {.arg alpha_curve} must be greater than 0.",
        "i" = "Use {.code alpha_curve = 1} for a linear opacity scale."
      ),
      call = call
    )
  }
  # Only steepness given: fill in the midpoint. A curve can only be linear
  # with midpoint 0.5, so steepness 1 gets 0.5 to keep `alpha_curve = 1`
  # meaning "linear"; otherwise use the S-curve default of 0.25.
  if (length(alpha_curve) == 1) {
    default_midpoint <- if (alpha_curve == 1) 0.5 else 0.25
    alpha_curve <- c(alpha_curve, default_midpoint)
  }
  if (alpha_curve[[2]] <= 0 || alpha_curve[[2]] >= 1) {
    cli::cli_abort(
      "The midpoint in {.arg alpha_curve} must be between 0 and 1 (exclusive).",
      call = call
    )
  }
  alpha_curve
}


#' Map heatmap values to opacity along an S-curve
#'
#' Values are first expressed relative to `max_value` (u in [0, 1], values
#' above the maximum are capped at 1), then passed through
#'
#'   f(u) = u^k / (u^k + (m / (1 - m))^k * (1 - u)^k)
#'
#' where k is the steepness and m the midpoint. f runs from 0 to 1 and passes
#' 0.5 at u = m. k = 1 with m = 0.5 is linear; higher k gives a steeper S, so
#' low values fade out and high values become more opaque. Finally f is
#' rescaled onto `alpha_scale`.
#'
#' @param values Numeric vector of heatmap values (NA allowed).
#' @param max_value The value that maps to the top of `alpha_scale`.
#' @param alpha_scale Numeric of length 2, opacity range `c(min, max)`.
#' @param alpha_curve Numeric of length 2, `c(steepness, midpoint)`, as
#'   returned by `check_alpha_curve()`.
#' @return Numeric vector of opacities, same length as `values`.
#' @noRd
map_alpha_curve <- function(values, max_value, alpha_scale, alpha_curve) {
  k <- alpha_curve[[1]]
  m <- alpha_curve[[2]]

  u <- pmin(pmax(values / max_value, 0), 1)
  f <- u^k / (u^k + (m / (1 - m))^k * (1 - u)^k)

  alpha_scale[[1]] + (alpha_scale[[2]] - alpha_scale[[1]]) * f
}


#' Quantile Binning Helper Function
#'
#' Creates quantile-based bins for numeric variables with customizable labels.
#' Adapted from the original my_quantile function.
#'
#' @param x Numeric vector to bin
#' @param n_groups Number of quantile groups to create
#' @param txt Label format: "pct" (percentages), "num" (numeric ranges),
#'   or "both" (markdown combined)
#' @return Factor with labeled bins
#' @noRd
pdr_quantile <- function(x, n_groups, txt = "pct") {
  # Calculate quantile breaks
  probs <- seq(0, 1, length.out = n_groups + 1)
  breaks <- quantile(x, probs = probs, na.rm = TRUE)

  # Handle duplicate breaks (if all values are the same)
  if (any(duplicated(breaks))) {
    cli::cli_warn(c(
      "Cannot create {n_groups} distinct quantile groups.",
      "i" = "Variable has insufficient variation."
    ))
    return(factor(rep("All values", length(x))))
  }

  # Create bins
  bins <- cut(x, breaks = breaks, include.lowest = TRUE)

  # Create labels based on format
  if (txt == "pct") {
    # Percentage labels
    pct_lower <- probs[-length(probs)] * 100
    pct_upper <- probs[-1] * 100

    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      if (i == 1) {
        labels[i] <- paste0("\u003c ", pct_upper[i] |> round(1), "%") # < (less-than)
      } else if (i == n_groups) {
        labels[i] <- paste0("\u2265 ", pct_lower[i] |> round(1), "%")
      } else {
        labels[i] <- paste0(
          pct_lower[i] |> round(1),
          "-",
          pct_upper[i] |> round(1),
          "%"
        )
      }
    }
  } else if (txt == "num") {
    # Numeric range labels
    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      labels[i] <- paste0(
        "(",
        round(breaks[i], 1),
        "-",
        round(breaks[i + 1], 1),
        ")"
      )
    }
    # Fix last bracket to be inclusive
    labels[n_groups] <- sub("\\)$", ")", labels[n_groups])
  } else if (txt == "both") {
    # Combined markdown labels
    pct_lower <- probs[-length(probs)] * 100
    pct_upper <- probs[-1] * 100

    labels <- character(n_groups)
    for (i in seq_len(n_groups)) {
      pct_part <- if (i == 1) {
        paste0("\u003c ", pct_upper[i] |> round(1), "%")
      } else if (i == n_groups) {
        paste0("\u2265 ", pct_lower[i] |> round(1), "%")
      } else {
        paste0(pct_lower[i] |> round(1), "-", pct_upper[i] |> round(1), "%")
      }

      num_part <- paste0(
        "(",
        round(breaks[i], 1),
        "-",
        round(breaks[i + 1], 1),
        ")"
      )

      labels[i] <- paste0("", pct_part, "<br>", num_part) # Markdown code here? e.g "**", pct_part, "**<br>"
    }
  } else {
    cli::cli_abort(
      "{.arg txt} must be one of {.val pct}, {.val num}, or {.val both}."
    )
  }

  # Apply labels
  levels(bins) <- labels

  return(bins)
}
