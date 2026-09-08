# Creates faceted density heatmaps showing how pain location patterns vary
#' across one or two stratification variables.
#'
#' @param .data A tibble with a single list-column of pain drawing data, as
#'   produced by [pdr_import_json()]. Each row holds a named list containing
#'   the drawing data for one recording. The name of that list-column is
#'   specified via `paindrawr_data`. All drawings must share the same canvas
#'   dimensions (`.width` and `.height`). Any additional columns (e.g.
#'   grouping variables) are preserved and available via `variables`.
#' @param paindrawr_data The name of the list-column in `.data` that contains
#'   the pain drawing data. Defaults to `pdr_data`. The column is unpacked with
#'   [tidyr::unnest_wider()] before processing; the resulting columns are
#'   expected to include `.id`, `.width`, `.height`, `.strokes`, `.points`,
#'   `.tool`, `.tool_width`, `.color`, `.alpha`, `.spray_radius`, and
#'   `.point_density`.
#' @param variables Character vector of length 1 or 2 specifying the grouping
#'   variable names present in `.data`. Numeric variables are binned into
#'   quantile groups; categorical variables use their existing levels. Use `NA`
#'   in either position to leave that faceting dimension empty (a hidden dummy
#'   variable will be inserted). Defaults to `c(NA, NA)`, producing a single
#'   unfaceted heatmap.
#' @param n_groups Integer. For numeric variables, specifies the number of
#'   quantile bins to create. A single value is recycled across all variables
#'   in `variables`. If both variables are numeric and different bin counts are
#'   desired, supply a vector of length 2 (e.g. `c(2, 3)`). Ignored for factor
#'   or character variables, which always use their existing levels. Default
#'   is `2`.
#' @param equal_n Logical. If `TRUE`, balances sample sizes across groups by
#'   sampling down to the size of the smallest group (capped at `max_n`).
#'   Default is `TRUE`.
#' @param max_n Integer. Maximum number of observations per group when
#'   `equal_n = TRUE`. Default is `1000`.
#' @param background_image Optional background image displayed behind the
#'   heatmap. Accepts either:
#'   * A file path to a PNG file (character string), or
#'   * A numeric array as returned by [png::readPNG()].
#'   Defaults to `NULL` (no background).
#' @param grid_size Integer. Coordinate binning resolution in pixels. Smaller
#'   values create finer grids but increase computation time. Default is `10`.
#'   Recommended range is 5–50 pixels.
#' @param point_size Numeric. Controls both the `size` and jitter `width`/
#'   `height` of points passed to [ggplot2::geom_jitter()]. Default is `0.25`.
#' @param alpha_scale Numeric vector of length 2 giving the `range` passed to
#'   [ggplot2::scale_alpha_continuous()]. Point opacity is mapped linearly from
#'   density onto this `c(min, max)` range: cells with the lowest density take
#'   `alpha_scale[1]` and the highest take `alpha_scale[2]`. Pass two equal values
#'   (e.g. `c(0.5, 0.5)`) to give every point the same opacity. Defaults to `c(0.1, 0.5)`.
#' @param color_scale Character or numeric. How to scale colours across facets:
#'   * `"max"` (default) — scales all facets to the global maximum density.
#'   * `"facet"` — each facet is scaled independently.
#'   * A numeric value — sets a specific upper limit for the colour scale.
#' @param label_format Character. Format for quantile group labels:
#'   * `"pct"` (default) — percentage ranges (e.g. `"< 50%"`).
#'   * `"num"` — numeric ranges (e.g. `"(1.0–5.5)"`).
#'   * `"both"` — combined markdown labels showing both formats, rendered via
#'     `<br>` line breaks.
#' @param show_n Logical. If `TRUE`, displays sample sizes as text labels
#'   inside each facet panel. Default is `FALSE`.
#' @param strips_in_markdown Logical. If `TRUE`, renders strip text using
#'   [ggtext::element_markdown()], enabling markdown and HTML formatting in
#'   facet labels. Also affects the plot subtitle and y-axis title. Default is
#'   `TRUE`.
#'
#' @return A [ggplot2::ggplot()] object with the faceted heatmap. The first
#'   variable in `variables` maps to the column facet (subtitle); the second
#'   maps to the row facet (y-axis label). Point colour encodes pain density
#'   (percentage of participants with a mark at each grid cell) using the
#'   `"plasma"` viridis palette.
#'
#' @details
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Validates inputs and unpacks the `paindrawr_data` list-column with
#'     [tidyr::unnest_wider()].
#'   \item Extracts canvas dimensions from `.width` and `.height`; all drawings
#'     must share identical dimensions.
#'   \item Inserts hidden dummy factor columns for any `NA` positions in
#'     `variables`, preserving the row/column slot assignment.
#'   \item Creates stratification groups for numeric
#'     variables (quantile binning) or [base::factor()] for categorical
#'     variables.
#'   \item Optionally balances sample sizes by sampling down to the smallest
#'     group size (up to `max_n`) using [dplyr::slice_sample()].
#'   \item Unnests `.strokes` and `.points` and joins them on `.id` and
#'     `.index`.
#'   \item Recreates spray effects for any rows where `.tool == "spray"` by
#'     expanding each point `point_density` times with a random offset within
#'     `spray_radius`.
#'   \item Bins coordinates into grid cells of size `grid_size` and calculates
#'     pain density as the percentage of participants with a mark at each
#'     location, per group.
#'   \item Renders a faceted heatmap using the `"plasma"` viridis colour scale.
#'     Point alpha is mapped from `pct` onto `alpha_scale` via
#'     [ggplot2::scale_alpha_continuous()].
#'   }
#'
#'
#' @section Warnings:
#' The function will warn when:
#' \itemize{
#'   \item More than 16 facets are created (may be slow).
#'   \item NA values are removed from grouping variables.
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
#' result <- pd |>
#'   pdr_plot_heatmap(
#'     variables = "pain_intensity",
#'     n_groups = 4,
#'     grid_size = 5,
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
#' @importFrom dplyr select mutate filter full_join join_by n_distinct slice_sample summarize all_of count left_join distinct pull n
#' @importFrom tidyr unnest_wider unnest uncount
#' @importFrom png readPNG
#' @importFrom scales percent
#' @importFrom stringr str_replace fixed
#' @importFrom rlang sym
#' @importFrom stats runif quantile
#' @importFrom ggplot2 ggplot aes coord_fixed theme_minimal scale_colour_viridis_c scale_alpha_continuous geom_jitter geom_text facet_wrap facet_grid annotation_raster labs theme element_rect element_blank element_text guide_colorbar guides vars label_wrap_gen
#' @importFrom ggtext element_markdown
#' @importFrom vctrs vec_ptype_abbr
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
  alpha_scale = c(0, 0.05),
  color_scale = "max",
  label_format = "pct",
  show_n = FALSE,
  strips_in_markdown = TRUE
) {
  # ============================================================================
  # STEP 1: Validate inputs
  # ============================================================================
  message("Validating inputs...")

  # Test alpha_scale arguments
  if (!is.numeric(alpha_scale) || length(alpha_scale) != 2) {
    stop("`alpha_scale` must be a numeric vector of length 2.", call. = FALSE)
  }
  if (any(alpha_scale < 0 | alpha_scale > 1)) {
    stop("`alpha_scale` values must be between 0 and 1.", call. = FALSE)
  }
  if (alpha_scale[[1]] > alpha_scale[[2]]) {
    stop(
      "The first `alpha_scale` value must be smaller or equal to the second alpha_scale value.",
      call. = FALSE
    )
  }

  # Unnest list
  .data <- .data |> tidyr::unnest_wider({{ paindrawr_data }})

  # Create dummy variables and levels if less than two variables are provided
  .DUMMY_COL <- factor("")

  if (length(variables) > 2) {
    stop("`variables` must be NULL, or of length 1 or 2.", call. = FALSE)
  }
  # Pad to length 2 WITHOUT removing NAs (preserves slot positions)
  variables <- c(variables, rep(NA_character_, 2 - length(variables)))

  # Recycle n_groups to match length of variables
  n_groups <- rep_len(n_groups, length(variables))

  # Fill each empty slot in place -> keeps user's row/col choice
  for (i in seq_along(variables)) {
    if (is.na(variables[i])) {
      dummy_name <- paste0(".DUMMY", i)
      .data[[dummy_name]] <- .DUMMY_COL
      variables[i] <- dummy_name
    }
  }

  # ============================================================================
  # STEP 2: Extract canvas dimensions
  # ============================================================================
  message("\nExtracting canvas dimensions...")

  image_width <- .data$.width |> unique()
  image_height <- .data$.height |> unique()

  if (length(image_width) != 1 || length(image_height) != 1) {
    stop(
      "Data must have single unique width and height values.\n",
      "You can only create heatmaps from one type of canvas at a time.",
      call. = FALSE
    )
  }

  # Validate and process background_image (same logic as pdr_recreate_drawing)
  if (!is.null(background_image)) {
    if (is.character(background_image)) {
      if (!file.exists(background_image)) {
        stop(
          "background_image file does not exist: ",
          background_image,
          call. = FALSE
        )
      }
      if (!grepl("\\.png$", background_image, ignore.case = TRUE)) {
        stop(
          "background_image must be a PNG file (ending in .png)",
          call. = FALSE
        )
      }
      background_image <- png::readPNG(background_image)
    } else if (!is.numeric(background_image) || !is.array(background_image)) {
      stop(
        "background_image must be either:\n",
        "  - A file path to a PNG image (character string)\n",
        "  - A raster array from png::readPNG() (numeric array)",
        call. = FALSE
      )
    }
  }

  # ============================================================================
  # STEP 3: Create stratification groups BEFORE unnesting
  # ============================================================================
  message("\nCreating stratification groups...")

  data_grouped <- stratify_data(
    .data = .data,
    variables = variables,
    n_groups = n_groups,
    label_format = label_format
  )

  # ============================================================================
  # STEP 4: Balance groups (if requested)
  # ============================================================================
  if (equal_n) {
    message("\nBalancing group sizes...")

    group_vars <- paste0(".VAR", seq_along(variables), "_grp")
    data_grouped <- balance_groups(
      .data = data_grouped,
      group_vars = group_vars,
      max_n = max_n
    )
  }

  # ============================================================================
  # STEP 5: Unnest coordinates
  # ============================================================================
  message("\nUnnesting coordinate data...")

  group_vars <- paste0(".VAR", seq_along(variables), "_grp")

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
    warning(
      "More than one tool detected: ",
      paste(tools_present, collapse = ", "),
      ".\n",
      "These are combined into one heatmap.\n",
      "We recommend only combining drawings from one tool.",
      call. = FALSE
    )
  }

  # ============================================================================
  # STEP 6: Recreate spray if needed
  # ============================================================================
  if ("spray" %in% tools_present) {
    message("\nRecreating spray effect...")
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
  # STEP 7: Calculate density
  # ============================================================================
  message("\nCalculating point density...")

  density_data <- calculate_pain_density(
    .data = coords_with_groups,
    group_vars = group_vars,
    grid_size = grid_size
  )

  # ============================================================================
  # STEP 8: Create plot
  # ============================================================================
  message("\nCreating heatmap plot...")

  # Change .DUMMY1 and 2 names to blank for more beautiful plotting
  variables <- variables |>
    stringr::str_replace(stringr::fixed(".DUMMY1"), " ") |>
    stringr::str_replace(stringr::fixed(".DUMMY2"), "  ")

  names(density_data) <- names(density_data) |>
    stringr::str_replace(stringr::fixed(".DUMMY1"), " ") |>
    stringr::str_replace(stringr::fixed(".DUMMY2"), "  ")

  # Plot
  plot_out <- create_heatmap_plot(
    density_data = density_data,
    variables = variables,
    background_image = background_image,
    image_width = image_width,
    image_height = image_height,
    n_vars = length(variables),
    point_size = point_size,
    alpha_scale = alpha_scale,
    color_scale = color_scale,
    show_n = show_n,
    strips_in_markdown = strips_in_markdown,
    label_format = label_format
  )

  # ============================================================================
  # STEP 9: Return
  # ============================================================================

  message("\nDone!")
  plot_out
}
