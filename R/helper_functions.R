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
      message("  ", na_count, " NA values removed from '", var_name, "'")
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
      message("  '", var_name, "' binned into ", n_grp, " quantile groups")
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

  message(
    "  Smallest group has n = ",
    min_group,
    "\n",
    "  Sampling n = ",
    target_n,
    " per group (max_n = ",
    max_n,
    ")"
  )

  # Warn if reducing groups
  if (target_n < min_group) {
    warning(
      "Rows removed to balance groups (n = ",
      target_n,
      " per group).\n",
      "Strip labels show ranges from full dataset, not reduced sample.",
      call. = FALSE
    )
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
#' @noRd
calculate_pain_density <- function(.data, group_vars, grid_size) {
  # Bin coordinates into grid
  data_binned <- .data |>
    dplyr::mutate(
      x_bin = round(.x / grid_size) * grid_size,
      y_bin = round(.y / grid_size) * grid_size
    )

  # Calculate total IDs per group
  group_totals <- data_binned |>
    dplyr::summarize(
      n_group = dplyr::n_distinct(.id),
      .by = dplyr::all_of(group_vars)
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


#' Create Heatmap Plot
#' @noRd
create_heatmap_plot <- function(
  density_data,
  variables,
  background_image,
  image_width,
  image_height,
  n_vars,
  point_size,
  alpha_scale,
  color_scale,
  show_n,
  label_format,
  strips_in_markdown
) {
  # Determine color scale limits
  if (is.character(color_scale)) {
    if (color_scale == "max") {
      scale_limits <- c(0, max(density_data$pct, na.rm = TRUE))
    } else {
      # "facet" - will use default (per-facet scaling)
      scale_limits <- NULL
    }
  } else {
    # Numeric value provided
    scale_limits <- c(0, color_scale)
  }

  # Build base plot
  p <- ggplot2::ggplot(density_data, ggplot2::aes(x = .x, y = .y))

  # Add background image if provided
  if (!is.null(background_image)) {
    p <- p +
      ggplot2::annotation_raster(
        background_image,
        xmin = 0,
        xmax = image_width,
        ymin = 0,
        ymax = image_height,
        interpolate = TRUE
      )
  }

  # Add density points
  p <- p +
    ggplot2::geom_jitter(
      ggplot2::aes(
        colour = pct,
        alpha = pct
      ),
      width = point_size,
      height = point_size,
      size = point_size
    ) +

    # Scale Alpha
    ggplot2::scale_alpha_continuous(range = alpha_scale, guide = "none")

  # Add color scale
  if (is.null(scale_limits)) {
    # Per-facet scaling
    p <- p +
      ggplot2::scale_colour_viridis_c(
        option = "plasma",
        direction = -1,
        begin = 0,
        end = 1,
        labels = scales::percent
      )
  } else {
    # Fixed scale across facets
    p <- p +
      ggplot2::scale_colour_viridis_c(
        option = "plasma",
        limits = scale_limits,
        direction = -1,
        begin = 0,
        end = 1,
        labels = scales::percent
      )
  }

  # Style color bar
  p <- p +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        title = "Share of participants",
        title.position = "top",
        barheight = 0.5,
        barwidth = 20,
        nbin = 50
      )
    )

  # Add faceting
  p <- p +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.VAR1_grp),
      rows = ggplot2::vars(.VAR2_grp),
      switch = "y",
      labeller = ggplot2::label_wrap_gen(width = 15, multi_line = TRUE)
    )

  # Add sample sizes if requested
  if (show_n) {
    n_data <- density_data |>
      dplyr::distinct(.VAR1_grp, .VAR2_grp, n_group) |>
      dplyr::mutate(
        label = paste0("n = ", n_group),
        x = image_width * 0.45,
        y = image_height * 0.1 # bottom-left, away from body
      )

    p <- p +
      ggplot2::geom_label(
        data = n_data,
        ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = 0,
        vjust = 0,
        size = 3,
        colour = "grey30",
        fontface = "bold"
      )
  }

  # Apply theme
  p <- p +
    ggplot2::labs(
      subtitle = paste0(variables[1]),
      y = paste0(variables[2])
    ) +
    ggplot2::coord_fixed(xlim = c(0, image_width), ylim = c(0, image_height)) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x.top = ggplot2::element_text(hjust = 0.5), # x axis title on top
      axis.title.x.bottom = ggplot2::element_blank(), # remove x axis title on bottom
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, vjust = 0.5)
    )

  # Add markdown formatting for strip text if using "both" label format
  if (label_format == "both") {
    if (requireNamespace("ggtext", quietly = TRUE)) {
      p <- p +
        ggplot2::theme(
          strip.text = ggtext::element_markdown()
        )
    } else {
      message(
        "  Install 'ggtext' package for better label formatting with label_format = 'both'"
      )
    }
  }

  if (strips_in_markdown) {
    p <- p +
      ggplot2::theme(
        strip.text.y.left = ggtext::element_markdown(angle = 45, vjust = 0.5),
        strip.text.x.top = ggtext::element_markdown(hjust = 0.5)
      )
  } else {
    p <- p +
      ggplot2::theme(
        strip.text.y.left = ggplot2::element_text(angle = 0, vjust = 0.5),
        strip.text.x.top = ggplot2::element_text(hjust = 0.5)
      )
  }

  return(p)
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
    warning(
      "Cannot create ",
      n_groups,
      " distinct quantile groups.\n",
      "Variable has insufficient variation.",
      call. = FALSE
    )
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
    stop("txt must be one of: 'pct', 'num', 'both'", call. = FALSE)
  }

  # Apply labels
  levels(bins) <- labels

  return(bins)
}
