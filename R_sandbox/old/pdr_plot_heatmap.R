#' Plot a pain heatmap from stroke and spray coordinates
#'
#' Creates faceted density heatmaps showing how pain location patterns vary
#' across one or two stratification variables. The raw stroke and spray
#' coordinates are binned into grid cells, and each point is coloured by the
#' share of participants in its group who marked that cell. For large datasets,
#' [pdr_plot_rgba_heatmap()] is faster and avoids overplotting.
#'
#' @param .data A tibble with a single list-column of pain drawing data, as
#'   produced by [pdr_import_json()]. Each row holds a named list containing
#'   the drawing data for one recording. All drawings must share the same
#'   canvas dimensions (`.width` and `.height`). Any additional columns (e.g.
#'   grouping variables) are preserved and available via `variables`.
#' @param paindrawr_data The name of the list-column in `.data` that contains
#'   the pain drawing data. Defaults to `pdr_data`. The column is unpacked with
#'   [tidyr::unnest_wider()]; the resulting columns are expected to include
#'   `.id`, `.width`, `.height`, `.strokes` and `.points`.
#' @param variables Character vector of length 1 or 2 specifying the grouping
#'   variable names present in `.data`. Numeric variables are binned into
#'   quantile groups; categorical variables use their existing levels. Use `NA`
#'   in either position to leave that faceting dimension empty. Defaults to
#'   `c(NA, NA)`, producing a single unfaceted heatmap.
#' @param n_groups Integer. For numeric variables, the number of quantile bins
#'   to create. A single value is recycled across `variables`; supply a vector
#'   of length 2 for different bin counts. Ignored for factor or character
#'   variables. Default is `2`.
#' @param equal_n Logical. If `TRUE` (default), balances sample sizes across
#'   groups by sampling down to the size of the smallest group (capped at
#'   `max_n`). Because every point is plotted, larger groups would otherwise
#'   look denser.
#' @param max_n Integer. Maximum number of drawings per group when
#'   `equal_n = TRUE`. Default is `1000`.
#' @param background_image Optional background image displayed behind the
#'   heatmap. Either a file path to a PNG file or a numeric array as returned
#'   by [png::readPNG()]. Defaults to `NULL` (no background).
#' @param grid_size Integer. Coordinate binning resolution in pixels. Smaller
#'   values create finer grids but increase computation time. Default is `10`.
#'   Recommended range is 5–50 pixels.
#' @param point_size Numeric. Controls both the `size` and jitter `width`/
#'   `height` of points passed to [ggplot2::geom_jitter()]. Default is `0.25`.
#' @param alpha_scale Numeric vector of length 2 giving the opacity range
#'   `c(min, max)` of each point. Points with value 0 get `min` and points at
#'   the scale maximum get `max`; `alpha_curve` controls the shape in between.
#'   Pass two equal values for constant opacity. Because overlapping points
#'   add up, the default `c(0, 0.3)` is lower than in [pdr_plot_rgba_heatmap()].
#'   A `min` above 0 gives every marked cell, including those marked by a
#'   single participant, a visible floor and works against the S-curve.
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
#'   The colour and its legend are not affected. See Details for the formula.
#' @param color_scale Character or numeric. How to scale colours across facets:
#'   * `"max"` (default) — one common scale from 0 to the global maximum.
#'     Facets are directly comparable.
#'   * `"facet"` — each facet is divided by its own maximum, so the colour
#'     shows values relative to the hottest cell in that facet. Facets are
#'     not comparable in absolute terms.
#'   * A single numeric value — sets a fixed upper limit for the scale, e.g.
#'     to give identical scales across plots.
#' @param label_format Character. Format for quantile group labels:
#'   * `"pct"` (default) — percentage ranges (e.g. `"< 50%"`).
#'   * `"num"` — numeric ranges (e.g. `"(1.0-5.5)"`).
#'   * `"both"` — combined markdown labels showing both formats, rendered via
#'     `<br>` line breaks.
#' @param show_n Logical. If `TRUE`, displays the number of drawings in each
#'   facet. Default is `FALSE`.
#' @param strips_in_markdown Logical. If `TRUE` (default), renders strip text
#'   using [ggtext::element_markdown()], enabling markdown and HTML formatting
#'   in facet labels.
#'
#' @return A [ggplot2::ggplot()] object with the faceted heatmap. The first
#'   variable in `variables` maps to the column facet; the second to the row
#'   facet. Point colour encodes the share of participants with a mark in each
#'   grid cell, using the `"plasma"` viridis palette.
#'
#' @details
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Validates inputs and unpacks the `paindrawr_data` list-column.
#'   \item Drops drawings whose `.strokes` is `NULL` or `NA`. Drawings with no
#'     strokes (nothing drawn) are kept and count towards the group size.
#'   \item Checks that all drawings share identical canvas dimensions.
#'   \item Creates stratification groups (quantile bins or factor levels).
#'   \item Optionally balances sample sizes by sampling down to the smallest
#'     group size (up to `max_n`).
#'   \item Unnests `.strokes` and `.points` and joins them on `.id` and
#'     `.index`.
#'   \item Recreates spray effects for strokes where `.tool == "spray"` by
#'     expanding each point `.point_density` times with a random offset within
#'     `.spray_radius`.
#'   \item Bins coordinates into grid cells of size `grid_size` and calculates,
#'     per group, the share of participants with a mark in each cell. Every
#'     point is kept, so densely drawn areas get many overlapping points.
#'   \item Renders the heatmap with [ggplot2::geom_jitter()].
#' }
#'
#' Opacity is mapped from each point's value `v` via `u = v / scale_max`
#' (capped at 1) and
#' \deqn{f(u) = \frac{u^k}{u^k + \left(\frac{m}{1-m}\right)^k (1-u)^k}}{f(u) = u^k / (u^k + (m / (1 - m))^k * (1 - u)^k)}
#' where `k` and `m` are the steepness and midpoint from `alpha_curve`. The
#' result is rescaled onto `alpha_scale`. The curve always passes through
#' 0 at `u = 0`, half opacity at `u = m`, and full opacity at `u = 1`. It is
#' linear only with `k = 1` and `m = 0.5`; with `k = 1` and `m < 0.5` it is
#' concave (low values boosted), with `m > 0.5` convex.
#'
#' @section Warnings:
#' The function will warn when:
#' \itemize{
#'   \item A numeric variable has insufficient variation to create distinct
#'     quantile groups.
#'   \item Both pen and spray tools are detected in the same dataset.
#'   \item `equal_n = TRUE` reduces any group below its original size (strip
#'     labels still reflect ranges from the full dataset).
#' }
#'
#' @examples
#' \dontrun{
#' # Two-way stratification
#' pd |>
#'   pdr_plot_heatmap(
#'     variables = c("age_group", "pain_duration"),
#'     n_groups = c(3, 2)
#'   )
#'
#' # Single variable with custom settings
#' pd |>
#'   pdr_plot_heatmap(
#'     variables = "pain_intensity",
#'     n_groups = 4,
#'     grid_size = 5,
#'     alpha_curve = 1,
#'     background_image = "inst/background.png"
#'   )
#'
#' # Use a custom list-column name
#' pd |>
#'   pdr_plot_heatmap(
#'     paindrawr_data = my_col,
#'     variables = "group"
#'   )
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n_distinct slice_sample summarize all_of left_join distinct pull n
#' @importFrom tidyr unnest_wider unnest uncount
#' @importFrom purrr map_lgl
#' @importFrom png readPNG
#' @importFrom scales percent
#' @importFrom stringr str_replace fixed
#' @importFrom stats runif quantile
#' @importFrom ggplot2 ggplot aes coord_fixed theme_minimal scale_colour_viridis_c scale_alpha_identity geom_jitter geom_label facet_grid annotation_raster labs theme element_rect element_blank element_text guide_colorbar guides vars label_wrap_gen
#' @importFrom ggtext element_markdown
#' @importFrom vctrs vec_ptype_abbr
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom rlang arg_match
#'
#' @export
pdr_plot_heatmap <- function(
  .data,
  paindrawr_data = pdr_data,
  variables = c(NA, NA),
  n_groups = 2,
  equal_n = TRUE,
  max_n = 1000,
  background_image = NULL,
  grid_size = 10,
  point_size = 0.25,
  alpha_scale = c(0, 0.1),
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

  label_format <- rlang::arg_match(label_format, c("pct", "num", "both"))

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

  # A floor above 0 makes even single-participant cells visible, which
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

  # ============================================================================
  # STEP 2: Drop drawings without stroke data
  # ============================================================================

  # A NULL or NA drawing or .strokes means "no drawing available" and should
  # not count towards the group size. A zero-row .strokes means "drew nothing"
  # and does. This is checked before unnest_wider(), which turns zero-row
  # elements into NULL and would make the two indistinguishable.
  strokes_missing <- purrr::map_lgl(
    dplyr::pull(.data, {{ paindrawr_data }}),
    \(drawing) {
      s <- if (is.list(drawing)) drawing$.strokes else NULL
      is.null(s) || (is.atomic(s) && all(is.na(s)))
    }
  )

  if (all(strokes_missing)) {
    cli::cli_abort("All {.field .strokes} values are NULL or NA.")
  }
  if (any(strokes_missing)) {
    cli::cli_inform(c(
      "i" = "{sum(strokes_missing)} drawing{?s} with NULL/NA {.field .strokes} removed."
    ))
    .data <- .data[!strokes_missing, ]
  }

  # Unnest list: one row per drawing
  .data <- .data |> tidyr::unnest_wider({{ paindrawr_data }})

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
  # STEP 4: Create stratification groups BEFORE unnesting
  # ============================================================================
  cli::cli_inform("Creating stratification groups...")

  data_grouped <- stratify_data(
    .data = .data,
    variables = variables,
    n_groups = n_groups,
    label_format = label_format
  )

  group_vars <- paste0(".VAR", seq_along(variables), "_grp")

  # ============================================================================
  # STEP 5: Balance groups (if requested)
  # ============================================================================
  if (equal_n) {
    cli::cli_inform("Balancing group sizes...")

    data_grouped <- balance_groups(
      .data = data_grouped,
      group_vars = group_vars,
      max_n = max_n
    )
  }

  # Count drawings per group here, before unnesting drops empty drawings
  group_totals <- data_grouped |>
    dplyr::summarize(
      n_group = dplyr::n(),
      .by = dplyr::all_of(group_vars)
    )

  # ============================================================================
  # STEP 6: Unnest coordinates
  # ============================================================================
  cli::cli_inform("Unnesting coordinate data...")

  pdr_s <- data_grouped |>
    dplyr::select(dplyr::all_of(c(".id", ".strokes", group_vars))) |>
    tidyr::unnest(cols = .strokes)

  pdr_p <- data_grouped |>
    dplyr::select(dplyr::all_of(c(".id", ".points"))) |>
    tidyr::unnest(cols = .points)

  coords_with_groups <- dplyr::full_join(
    pdr_s,
    pdr_p,
    by = dplyr::join_by(.id, .index)
  )

  # Check for multiple tools
  tools_present <- coords_with_groups$.tool |> unique()

  if (length(tools_present) > 1) {
    cli::cli_warn(c(
      "More than one tool detected: {.val {tools_present}}.",
      "i" = "These are combined into one heatmap.",
      "i" = "We recommend only combining drawings from one tool."
    ))
  }

  # ============================================================================
  # STEP 7: Recreate spray if needed
  # ============================================================================
  if ("spray" %in% tools_present) {
    cli::cli_inform("Recreating spray effect...")
    coords_with_groups_spray <- coords_with_groups |>
      dplyr::filter(.tool == "spray")
    coords_with_groups_spray <- recreate_spray(coords_with_groups_spray)

    # Recombine expanded spray data
    coords_with_groups <- coords_with_groups |>
      dplyr::filter(.tool != "spray") |>
      dplyr::full_join(coords_with_groups_spray)

    rm(coords_with_groups_spray)
  }

  # ============================================================================
  # STEP 8: Calculate density
  # ============================================================================
  cli::cli_inform("Calculating point density...")

  density_data <- calculate_pain_density(
    .data = coords_with_groups,
    group_totals = group_totals,
    group_vars = group_vars,
    grid_size = grid_size
  )

  # For color_scale = "facet", rescale each group to its own maximum
  if (identical(color_scale, "facet")) {
    density_data <- density_data |>
      dplyr::mutate(pct = pct / max(pct), .by = dplyr::all_of(group_vars))
  }

  # ============================================================================
  # STEP 9: Create plot
  # ============================================================================
  cli::cli_inform("Creating heatmap plot...")

  # Change .DUMMY1 and 2 names to blank for more beautiful plotting
  variables <- variables |>
    stringr::str_replace(stringr::fixed(".DUMMY1"), " ") |>
    stringr::str_replace(stringr::fixed(".DUMMY2"), "  ")

  names(density_data) <- names(density_data) |>
    stringr::str_replace(stringr::fixed(".DUMMY1"), " ") |>
    stringr::str_replace(stringr::fixed(".DUMMY2"), "  ")

  legend_title <- if (identical(color_scale, "facet")) {
    "Relative to facet maximum"
  } else {
    "Share of participants"
  }

  plot_out <- create_heatmap_plot(
    density_data = density_data,
    variables = variables,
    background_image = background_image,
    image_width = image_width,
    image_height = image_height,
    point_size = point_size,
    alpha_scale = alpha_scale,
    alpha_curve = alpha_curve,
    color_scale = color_scale,
    legend_title = legend_title,
    show_n = show_n,
    label_format = label_format,
    strips_in_markdown = strips_in_markdown
  )

  # ============================================================================
  # STEP 10: Return
  # ============================================================================
  cli::cli_inform("Done!")
  plot_out
}


#' Create Heatmap Plot
#' @noRd
create_heatmap_plot <- function(
  density_data,
  variables,
  background_image,
  image_width,
  image_height,
  point_size,
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
  # (alpha_curve) can be applied
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

  # Add density points
  p <- p +
    ggplot2::geom_jitter(
      ggplot2::aes(colour = pct, alpha = .alpha),
      width = point_size,
      height = point_size,
      size = point_size
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_colour_viridis_c(
      option = "plasma",
      limits = scale_limits,
      direction = -1,
      begin = 0,
      end = 1,
      labels = scales::percent
    ) +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
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

  return(p)
}
