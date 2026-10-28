#' Plot a pain heatmap from rendered RGBA rasters
#'
#' Creates faceted heatmaps showing how pain location patterns vary across one
#' or two stratification variables. Unlike [pdr_plot_heatmap()], which bins the
#' raw stroke and spray coordinates, this function works on the rendered
#' `.rgba` rasters added by [pdr_add_rgba()]. Each drawing is reduced to its
#' alpha channel, drawings are summed pixel-wise within each group, and the sum
#' is divided by the number of drawings in the group. Because every group is
#' expressed relative to its own size, groups of very different sizes can be
#' compared directly and no balancing/downsampling is needed.
#'
#' @param .data A tibble with a list-column of pain drawing data, as produced
#'   by [pdr_import_json()], where each drawing contains an `.rgba` element
#'   (see [pdr_add_rgba()]). All drawings must share the same canvas
#'   dimensions (`.width` and `.height`). Any additional columns (e.g.
#'   grouping variables) are preserved and available via `variables`.
#' @param paindrawr_data The name of the list-column in `.data` that contains
#'   the pain drawing data. Defaults to `pdr_data`.
#' @param variables Character vector of length 1 or 2 specifying the grouping
#'   variable names present in `.data`. Numeric variables are binned into
#'   quantile groups; categorical variables use their existing levels. Use `NA`
#'   in either position to leave that faceting dimension empty. Defaults to
#'   `c(NA, NA)`, producing a single unfaceted heatmap.
#' @param n_groups Integer. For numeric variables, the number of quantile bins
#'   to create. A single value is recycled across `variables`; supply a vector
#'   of length 2 for different bin counts. Ignored for factor or character
#'   variables. Default is `2`.
#' @param method Character. How each drawing's alpha channel contributes to
#'   the heatmap:
#'   * `"binary"` (default) — every pixel with alpha above `alpha_threshold`
#'     counts as 1. The heatmap shows the share of participants who marked
#'     each pixel.
#'   * `"intensity"` — pixels above `alpha_threshold` keep their alpha value.
#'     The heatmap shows the mean drawing intensity per pixel, so densely
#'     drawn areas weigh more.
#' @param alpha_threshold Numeric in `[0, 1)`. Pixels with alpha less than or
#'   equal to this value are set to 0 before summing. The default `0.05`
#'   removes most of the anti-aliased fringe around strokes (pixels only
#'   partially covered by a stroke) while keeping pixels that are at least
#'   roughly half covered by a stroke with `.alpha = 0.1`. A warning is given
#'   if the threshold is above half of the lowest stroke `.alpha` in the
#'   data, as real strokes may then be removed.
#' @param background_image Optional background image displayed behind the
#'   heatmap. Either a file path to a PNG file or a numeric array as returned
#'   by [png::readPNG()]. Defaults to `NULL` (no background).
#' @param alpha_scale Numeric vector of length 2 giving the opacity range
#'   `c(min, max)`. Cells with value 0 get `min` and cells at the scale
#'   maximum get `max`; `alpha_curve` controls the shape in between. Pass two
#'   equal values for constant opacity. Defaults to `c(0, 1)`. A `min` above
#'   0 gives every marked pixel, including those marked by a single
#'   participant, a visible floor and works against the S-curve.
#' @param alpha_curve Numeric of length 1 or 2, `c(steepness, midpoint)`,
#'   controlling how heatmap values are mapped to opacity within
#'   `alpha_scale`.
#'   * Steepness controls how S-shaped the mapping is. Higher values fade low
#'     values more and make high values more opaque.
#'   * The midpoint is the fraction of the scale maximum at which opacity
#'     reaches half of `alpha_scale`. Lower values let moderate areas show
#'     more; higher values show only the hotspots. Moving the midpoint away
#'     from `0.5` bends the curve at any steepness, including `1`.
#'
#'   Defaults to `c(3, 0.25)`. If only a steepness is given, the midpoint is
#'   `0.25`, except for `alpha_curve = 1`, which uses `0.5` and gives a linear
#'   scale (equivalent to `c(1, 0.5)`).
#'
#'   The colour (fill) and its legend are not affected. See Details for the
#'   formula.
#' @param color_scale Character or numeric. How to scale colours across facets:
#'   * `"max"` (default) — one common scale from 0 to the global maximum.
#'     Facets are directly comparable.
#'   * `"facet"` — each facet is divided by its own maximum, so the colour
#'     shows values relative to the hottest pixel in that facet. Facets are
#'     not comparable in absolute terms.
#'   * A single numeric value — sets a fixed upper limit for the scale; values
#'     above it are shown in the top colour.
#' @param label_format Character. Format for quantile group labels: `"pct"`
#'   (default), `"num"`, or `"both"`. See [pdr_plot_heatmap()].
#' @param show_n Logical. If `TRUE`, displays the number of drawings in each
#'   facet. Default is `FALSE`.
#' @param strips_in_markdown Logical. If `TRUE` (default), renders strip text
#'   using [ggtext::element_markdown()].
#'
#' @return A [ggplot2::ggplot()] object with the faceted heatmap. The first
#'   variable in `variables` maps to the column facet; the second to the row
#'   facet.
#'
#' @details
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Validates inputs and unpacks the `paindrawr_data` list-column.
#'   \item Checks that `.rgba` exists and drops drawings whose `.rgba` is
#'     `NULL` or `NA`. Drawings with an all-zero `.rgba` (nothing drawn) are
#'     kept and count towards the group size.
#'   \item Creates stratification groups (quantile bins or factor levels).
#'   \item Reduces each `.rgba` to its alpha channel, applies
#'     `alpha_threshold`, and binarises it (`method = "binary"`).
#'   \item Sums the alpha matrices within each group and divides by the
#'     number of drawings in the group.
#'   \item Converts each group matrix to a long tibble of pixel cells
#'     (zero cells become `NA` and are drawn transparent).
#'   \item Renders the heatmap with [ggplot2::geom_raster()].
#' }
#'
#' Opacity is mapped from each cell's value `v` via `u = v / scale_max`
#' (capped at 1) and
#' \deqn{f(u) = \frac{u^k}{u^k + \left(\frac{m}{1-m}\right)^k (1-u)^k}}{f(u) = u^k / (u^k + (m / (1 - m))^k * (1 - u)^k)}
#' where `k` and `m` are the steepness and midpoint from `alpha_curve`. The
#' result is rescaled onto `alpha_scale`. The curve always passes through
#' 0 at `u = 0`, half opacity at `u = m`, and full opacity at `u = 1`. It is
#' linear only with `k = 1` and `m = 0.5`; with `k = 1` and `m < 0.5` it is
#' concave (low values boosted), with `m > 0.5` convex.
#'
#' The heatmap inherits the rendering choices made in [pdr_add_rgba()]
#' (e.g. `polygon` mode and stroke widths).
#'
#' @examples
#' \dontrun{
#' pd |>
#'   dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data)) |>
#'   pdr_plot_rgba_heatmap(variables = c("age_group", "sex"))
#'
#' # Weight pixels by drawing intensity
#' pd |>
#'   pdr_plot_rgba_heatmap(
#'     variables = "pain_intensity",
#'     method = "intensity",
#'     background_image = "inst/background.png"
#'   )
#' }
#'
#' @importFrom dplyr select mutate filter summarize all_of distinct
#' @importFrom tidyr unnest_wider unnest
#' @importFrom purrr map map_lgl map2
#' @importFrom tibble tibble
#' @importFrom png readPNG
#' @importFrom scales percent squish
#' @importFrom stringr str_replace fixed
#' @importFrom ggplot2 ggplot aes geom_raster annotation_raster scale_fill_viridis_c scale_alpha_identity guides guide_colorbar facet_grid vars label_wrap_gen geom_label labs coord_fixed theme_minimal theme element_rect element_blank element_text
#' @importFrom ggtext element_markdown
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom rlang arg_match
#'
#' @export
pdr_plot_rgba_heatmap <- function(
  .data,
  paindrawr_data = pdr_data,
  variables = c(NA, NA),
  n_groups = 2,
  method = "binary",
  alpha_threshold = 0.05,
  background_image = NULL,
  alpha_scale = c(0, 1),
  alpha_curve = c(3, 0.25),
  color_scale = "max",
  label_format = "pct",
  show_n = FALSE,
  strips_in_markdown = TRUE
) {
  # ============================================================================
  # STEP 1: Validate inputs
  # ============================================================================
  cli::cli_inform("Validating inputs...")

  method <- rlang::arg_match(method, c("binary", "intensity"))
  label_format <- rlang::arg_match(label_format, c("pct", "num", "both"))

  # Test alpha_threshold
  if (
    !is.numeric(alpha_threshold) ||
      length(alpha_threshold) != 1 ||
      is.na(alpha_threshold) ||
      alpha_threshold < 0 ||
      alpha_threshold >= 1
  ) {
    cli::cli_abort(
      "{.arg alpha_threshold} must be a single number in [0, 1)."
    )
  }

  # Test alpha_scale arguments
  if (!is.numeric(alpha_scale) || length(alpha_scale) != 2) {
    cli::cli_abort("{.arg alpha_scale} must be a numeric vector of length 2.")
  }
  if (any(alpha_scale < 0 | alpha_scale > 1)) {
    cli::cli_abort("{.arg alpha_scale} values must be between 0 and 1.")
  }
  if (alpha_scale[[1]] > alpha_scale[[2]]) {
    cli::cli_abort(
      "The first {.arg alpha_scale} value must be smaller than or equal to the second."
    )
  }

  # Test alpha_curve and complete it to c(steepness, midpoint)
  alpha_curve <- check_alpha_curve(alpha_curve)

  # A floor above 0 makes even single-participant pixels visible, which
  # works against the S-curve fading low values out
  if (alpha_scale[[1]] > 0 && alpha_curve[[1]] != 1) {
    cli::cli_inform(c(
      "i" = "{.arg alpha_scale} has a minimum above 0 ({alpha_scale[[1]]}); low values will not fade out completely."
    ))
  }

  # Test color_scale: "max", "facet", or a single positive number
  color_scale_ok <- (is.character(color_scale) &&
    length(color_scale) == 1 &&
    color_scale %in% c("max", "facet")) ||
    (is.numeric(color_scale) &&
      length(color_scale) == 1 &&
      !is.na(color_scale) &&
      color_scale > 0)
  if (!color_scale_ok) {
    cli::cli_abort(
      "{.arg color_scale} must be {.val max}, {.val facet}, or a single positive number."
    )
  }

  if (length(variables) > 2) {
    cli::cli_abort("{.arg variables} must be NULL, or of length 1 or 2.")
  }

  # Unnest list: one row per drawing, with .rgba, .width, .height, .strokes
  # etc. as columns next to the user's grouping variables
  .data <- .data |> tidyr::unnest_wider({{ paindrawr_data }})

  # ============================================================================
  # STEP 2: Check .rgba and drop drawings without one
  # ============================================================================
  cli::cli_inform("Checking .rgba data...")

  if (!".rgba" %in% names(.data)) {
    cli::cli_abort(c(
      "No {.field .rgba} found in the paindrawr data.",
      "i" = "Add it first with {.fn pdr_add_rgba}, e.g. {.code mutate(pdr_data = pdr_add_rgba(pdr_data))}."
    ))
  }

  # A NULL or NA .rgba means "no drawing available" and should not count
  # towards the group size. An all-zero .rgba means "drew nothing" and does.
  rgba_missing <- purrr::map_lgl(
    .data$.rgba,
    \(rgba) is.null(rgba) || all(is.na(rgba))
  )

  if (all(rgba_missing)) {
    cli::cli_abort("All {.field .rgba} values are NULL or NA.")
  }
  if (any(rgba_missing)) {
    cli::cli_inform(c(
      "i" = "{sum(rgba_missing)} drawing{?s} with NULL/NA {.field .rgba} removed."
    ))
    .data <- .data[!rgba_missing, ]
  }

  # Warn if the threshold could remove real strokes, not just anti-aliasing.
  # A pixel fully covered by one stroke has alpha ~= the stroke's .alpha, so a
  # threshold above half of that starts to eat into the strokes.
  if (".strokes" %in% names(.data)) {
    stroke_alphas <- purrr::map(.data$.strokes, \(s) s$.alpha) |> unlist()
    if (length(stroke_alphas) > 0) {
      min_stroke_alpha <- min(stroke_alphas, na.rm = TRUE)
      if (alpha_threshold > 0.5 * min_stroke_alpha) {
        cli::cli_warn(c(
          "{.arg alpha_threshold} ({alpha_threshold}) is above half of the lowest stroke {.field .alpha} ({min_stroke_alpha}).",
          "!" = "Parts of actual strokes may be removed.",
          "i" = "Consider a threshold of {0.5 * min_stroke_alpha} or lower."
        ))
      }
    }
  }

  # ============================================================================
  # STEP 3: Create dummy variables and extract canvas dimensions
  # ============================================================================

  # Pad to length 2 WITHOUT removing NAs (preserves slot positions)
  variables <- c(variables, rep(NA_character_, 2 - length(variables)))

  # Recycle n_groups to match length of variables
  n_groups <- rep_len(n_groups, length(variables))

  # Fill each empty slot in place -> keeps user's row/col choice
  .DUMMY_COL <- factor("")
  for (i in seq_along(variables)) {
    if (is.na(variables[i])) {
      dummy_name <- paste0(".DUMMY", i)
      .data[[dummy_name]] <- .DUMMY_COL
      variables[i] <- dummy_name
    }
  }

  cli::cli_inform("Extracting canvas dimensions...")

  image_width <- .data$.width |> unique()
  image_height <- .data$.height |> unique()

  if (length(image_width) != 1 || length(image_height) != 1) {
    cli::cli_abort(c(
      "Data must have a single unique {.field .width} and {.field .height}.",
      "x" = "Found {length(image_width)} width{?s} and {length(image_height)} height{?s}.",
      "i" = "You can only create heatmaps from one type of canvas at a time."
    ))
  }

  # Validate and read background_image (NULL, PNG path, or raster array)
  background_image <- read_background_image(background_image)

  # ============================================================================
  # STEP 4: Create stratification groups
  # ============================================================================
  cli::cli_inform("Creating stratification groups...")

  # Adds .VAR1_grp and .VAR2_grp factor columns; rows with NA in a grouping
  # variable are removed
  data_grouped <- stratify_data(
    .data = .data,
    variables = variables,
    n_groups = n_groups,
    label_format = label_format
  )

  group_vars <- paste0(".VAR", seq_along(variables), "_grp")

  # ============================================================================
  # STEP 5: Sum alpha matrices within each group
  # ============================================================================
  cli::cli_inform("Summing alpha channels per group...")

  # Data is one row per drawing. Collapse to one row per group, where
  # alpha_sum is a single height x width matrix: the sum of every drawing's
  # thresholded (and, for "binary", 0/1) alpha channel. Reduce() accumulates
  # one drawing at a time so we never hold all matrices in memory at once.
  group_sums <- data_grouped |>
    dplyr::summarize(
      n_group = dplyr::n(),
      alpha_sum = list(
        Reduce(
          \(acc, rgba) acc + rgba_to_alpha(rgba, method, alpha_threshold),
          .rgba,
          matrix(0, nrow = image_height, ncol = image_width)
        )
      ),
      .by = dplyr::all_of(group_vars)
    )

  # Divide by group size: "binary" gives the share of participants who marked
  # each pixel, "intensity" gives the mean alpha per pixel
  group_sums <- group_sums |>
    dplyr::mutate(
      alpha_matrix = purrr::map2(alpha_sum, n_group, \(m, n) m / n)
    )

  # For color_scale = "facet", rescale each group to its own maximum
  if (identical(color_scale, "facet")) {
    group_sums <- group_sums |>
      dplyr::mutate(
        alpha_matrix = purrr::map(alpha_matrix, \(m) {
          if (max(m) > 0) m / max(m) else m
        })
      )
  }

  # ============================================================================
  # STEP 6: Convert matrices to long format
  # ============================================================================
  cli::cli_inform("Converting to plotting data...")

  # One row per group -> one row per pixel per group, with .x, .y and pct.
  # The full grid is kept (zeros as NA) so geom_raster sees evenly spaced cells.
  density_data <- group_sums |>
    dplyr::mutate(cells = purrr::map(alpha_matrix, alpha_matrix_to_tibble)) |>
    dplyr::select(-alpha_sum, -alpha_matrix) |>
    tidyr::unnest(cols = cells)

  # ============================================================================
  # STEP 7: Create plot
  # ============================================================================
  cli::cli_inform("Creating heatmap plot...")

  # Change .DUMMY1 and 2 names to blank for more beautiful plotting
  variables <- variables |>
    stringr::str_replace(stringr::fixed(".DUMMY1"), " ") |>
    stringr::str_replace(stringr::fixed(".DUMMY2"), "  ")

  legend_title <- if (identical(color_scale, "facet")) {
    "Relative to facet maximum"
  } else if (method == "binary") {
    "Share of participants"
  } else {
    "Mean intensity"
  }

  plot_out <- create_rgba_heatmap_plot(
    density_data = density_data,
    variables = variables,
    background_image = background_image,
    image_width = image_width,
    image_height = image_height,
    alpha_scale = alpha_scale,
    alpha_curve = alpha_curve,
    color_scale = color_scale,
    legend_title = legend_title,
    show_n = show_n,
    label_format = label_format,
    strips_in_markdown = strips_in_markdown
  )

  # ============================================================================
  # STEP 8: Return
  # ============================================================================
  cli::cli_inform("Done!")
  plot_out
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Reduce an RGBA array to a thresholded alpha matrix
#'
#' @param rgba Numeric array, height x width x 4.
#' @param method "binary" (pixels above threshold become 1) or "intensity"
#'   (pixels above threshold keep their alpha).
#' @param alpha_threshold Pixels with alpha <= this value become 0.
#' @return Numeric matrix, height x width.
#' @noRd
rgba_to_alpha <- function(rgba, method, alpha_threshold) {
  alpha <- rgba[,, 4]
  alpha[is.na(alpha)] <- 0
  alpha[alpha <= alpha_threshold] <- 0
  if (method == "binary") {
    alpha <- (alpha > 0) * 1
  }
  alpha
}


#' Convert a height x width matrix to a long tibble of raster cells
#'
#' Matrix row 1 is the top of the image, while ggplot's y axis points up, so
#' rows are flipped. Cell centres are placed at half-pixel offsets so the
#' raster spans exactly [0, width] x [0, height], matching the background.
#' Zero cells are set to NA so they are drawn transparent.
#'
#' @param m Numeric matrix.
#' @return A tibble with columns .x, .y and pct.
#' @noRd
alpha_matrix_to_tibble <- function(m) {
  m[m == 0] <- NA
  tibble::tibble(
    .x = rep(seq_len(ncol(m)), each = nrow(m)) - 0.5,
    .y = rep(rev(seq_len(nrow(m))), times = ncol(m)) - 0.5,
    pct = as.vector(m)
  )
}


#' Create the RGBA heatmap plot
#' @noRd
create_rgba_heatmap_plot <- function(
  density_data,
  variables,
  background_image,
  image_width,
  image_height,
  alpha_scale,
  alpha_curve,
  color_scale,
  legend_title,
  show_n,
  label_format,
  strips_in_markdown
) {
  # Determine scale limits. With "facet" the values are already rescaled to
  # [0, 1] per group, so a common 0-1 scale is used.
  scale_limits <- if (identical(color_scale, "max")) {
    c(0, max(density_data$pct, na.rm = TRUE))
  } else if (identical(color_scale, "facet")) {
    c(0, 1)
  } else {
    c(0, color_scale)
  }

  # Opacity is computed here rather than by a ggplot scale, so the S-curve
  # (alpha_curve) can be applied. Values at or above the scale maximum get
  # the top of alpha_scale.
  density_data$.alpha <- map_alpha_curve(
    density_data$pct,
    max_value = scale_limits[[2]],
    alpha_scale = alpha_scale,
    alpha_curve = alpha_curve
  )

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

  # Add heatmap raster; NA (zero) cells are dropped and stay transparent
  p <- p +
    ggplot2::geom_raster(
      ggplot2::aes(fill = pct, alpha = .alpha),
      na.rm = TRUE
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      direction = -1,
      limits = scale_limits,
      oob = scales::squish,
      na.value = NA,
      labels = scales::percent
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = legend_title,
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
        y = image_height * 0.1
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
    ggplot2::coord_fixed(
      xlim = c(0, image_width),
      ylim = c(0, image_height),
      expand = FALSE
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = "white"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, vjust = 0.5)
    )

  # Markdown strip text for "both" label format
  if (label_format == "both") {
    p <- p + ggplot2::theme(strip.text = ggtext::element_markdown())
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

  p
}
