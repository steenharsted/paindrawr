#' Recreate a pain drawing as a ggplot
#'
#' Renders stroke and spray data from a pain drawing tibble into a ggplot,
#' optionally composited with a background image. When `rasterize = TRUE`
#' (default), the plot is rasterized and returned as a pixel-accurate raster
#' layer — preserving exact canvas proportions. When
#' `rasterize = FALSE`, the raw ggplot with vector strokes is returned instead.
#'
#' Multiple drawings are arranged with [ggplot2::facet_wrap()]. A warning is
#' issued when more than 9 drawings are passed, as rendering may be slow.
#'
#' Missing drawings (`NA` or `NULL` entries in the list-column) are dropped
#' with a warning; an error is raised only if no drawings remain.
#'
#' @param .data A tibble with a single list-column of pain drawing data, as
#'   produced by [pdr_import_json()]. Each row holds a named list containing
#'   the drawing data for one recording. The name of that list-column is
#'   specified via `paindrawr_data`. All drawings must share the same canvas
#'   dimensions (`.width` and `.height`).
#' @param paindrawr_data The name of the list-column in `.data` that contains
#'   the pain drawing data. Defaults to `pdr_data`. The column is unpacked with
#'   [tidyr::unnest_wider()] before processing; the resulting columns are
#'   expected to include `.id`, `.width`, `.height`, `.strokes`, `.points`,
#'   `.tool`, `.tool_width`, `.color`, `.alpha`, `.spray_radius`, and
#'   `.point_density`.
#' @param background_image Optional background image displayed behind the
#'   strokes. Accepts either:
#'   * A file path to a PNG file (character string), or
#'   * A numeric array as returned by [png::readPNG()].
#'
#'   Defaults to `NULL` (no background).
#' @param include_id Logical. If `TRUE`, the `.id` value is shown as a facet
#'   strip label above each panel. Default is `FALSE`.
#' @param rasterize Logical. If `TRUE` (default), the plot is rasterized and
#'   returned as a new ggplot with [ggplot2::annotation_raster()]. The output
#'   is pixel-accurate with a locked aspect ratio. If `FALSE`, the raw ggplot
#'   with vector strokes is returned.
#' @param method Character. Controls the rasterization back-end when
#'   `rasterize = TRUE`. Either:
#'   * `"memory"` (default) — renders in-memory via [ragg::agg_capture()],
#'     no temp file is written to disk.
#'   * `"file"` — saves to a temporary PNG via [ggplot2::ggsave()] and reads
#'     it back with [png::readPNG()]. Use this if the `"memory"` path produces
#'     unexpected rendering artefacts.
#' @param clean_up Logical. If `TRUE` (default), the temporary PNG file is
#'   deleted on exit. Only relevant when `method = "file"`. Set to `FALSE` to
#'   retain the file for debugging.
#' @param dpi Resolution used during rasterization. Defaults to `96`, matching
#'   the CSS pixel density of the web canvas where drawings are collected —
#'   this ensures a 1:1 pixel correspondence between the original drawing and
#'   the output. Increase for print-quality output (e.g. `300`), noting this
#'   does not affect the canvas dimensions, only output pixel density.
#' @param type A string set to either `"path"` or `"polygon"`. The former
#'   (default) renders strokes as open paths. The latter renders strokes as
#'   closed polygons filled in black, which is useful for generating anatomy
#'   area templates.
#'
#' @return A [ggplot2::ggplot()] object. When `rasterize = TRUE`, the plot
#'   contains a single [ggplot2::annotation_raster()] layer with a locked
#'   aspect ratio. When `rasterize = FALSE`, the plot contains vector stroke
#'   layers and supports further ggplot2 additions.
#'
#' @seealso [pdr_import_json()] to read pain drawing JSON files,
#'   [pdr_add_rgba()] to store raster arrays as a column in the tibble,
#'
#' @examples
#' \dontrun{
#' pd <- pdr_import_json(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))
#'
#' # Default: in-memory rasterization with background
#' pd |> pdr_plot_drawing(background_image = "inst/background.png")
#'
#' # File-based rasterization
#' pd |> pdr_plot_drawing(method = "file")
#'
#' # Vector output — can add further ggplot layers
#' pd |> pdr_plot_drawing(rasterize = FALSE)
#'
#' # Show id labels in facet strips
#' pd |> pdr_plot_drawing(include_id = TRUE)
#'
#' # Use a custom list-column name
#' pd |> pdr_plot_drawing(paindrawr_data = my_col)
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n
#' @importFrom tidyr unnest unnest_wider uncount
#' @importFrom png readPNG
#' @importFrom ragg agg_capture
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void theme element_blank scale_color_identity scale_size_identity scale_alpha_identity scale_linewidth_identity geom_path geom_point facet_wrap annotation_raster ggsave
#' @importFrom cli cli_warn cli_abort
#'
#' @export
pdr_plot_drawing <- function(
  .data,
  paindrawr_data = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  rasterize = TRUE,
  method = "memory",
  clean_up = TRUE,
  dpi = 96,
  type = "path"
) {
  # Drop missing drawings (NA or NULL entries) before unnesting
  drawings <- dplyr::pull(.data, {{ paindrawr_data }})
  is_valid <- vapply(
    drawings,
    function(x) is.list(x) && length(x) > 0,
    logical(1)
  )
  n_missing <- sum(!is_valid)
  if (n_missing > 0) {
    cli::cli_warn("Dropping {n_missing} missing drawing{?s}.")
    .data <- .data[is_valid, , drop = FALSE]
  }
  if (nrow(.data) == 0) {
    cli::cli_abort("No non-missing drawings to plot.")
  }

  # Unnest list
  .data <- .data |> tidyr::unnest_wider({{ paindrawr_data }})

  # Validate method argument
  method <- match.arg(method, choices = c("memory", "file"))

  # Test that we only 1 height and width value, respectively
  if (
    (.data$.width |> unique() |> length() > 1) ||
      (.data$.height |> unique() |> length() > 1)
  ) {
    cli::cli_abort(
      "All drawings must share the same canvas dimensions, but multiple
      values of {.field .width} or {.field .height} were found. Process one canvas
      size at a time."
    )
  }

  # Validate and process background_image
  if (!is.null(background_image)) {
    if (is.character(background_image)) {
      # It's a file path
      if (!file.exists(background_image)) {
        cli::cli_abort(
          "Background image file does not exist: {.path {background_image}}"
        )
      }
      if (!grepl("\\.png$", background_image, ignore.case = TRUE)) {
        cli::cli_abort(
          "Background image must be a PNG file (ending in {.val .png})."
        )
      }
      background_image <- png::readPNG(background_image)
    } else if (!is.numeric(background_image) || !is.array(background_image)) {
      # Not a numeric array (expected output from png::readPNG)
      cli::cli_abort(
        "{.arg background_image} must be either a file path to a PNG image
        (character string) or a raster array from {.fn png::readPNG}."
      )
    }
  }

  # Extract number of ids
  id_n <- nrow(.data)

  # Warn user if numbers of ids > 9
  if (id_n > 9) {
    cli::cli_warn(
      "Recreating {id_n} drawings. This may be slow for large numbers of panels."
    )
  }

  # Extract height and width
  image_width <- .data$.width |> unique()
  image_height <- .data$.height |> unique()

  # Unnest and Join the s and p columns
  pdr_s <- .data |>
    dplyr::select(.id, .strokes) |>
    tidyr::unnest(cols = .strokes)

  pdr_p <- .data |>
    dplyr::select(.id, .points) |>
    tidyr::unnest(cols = .points)

  .data <- dplyr::full_join(pdr_s, pdr_p, by = dplyr::join_by(.id, .index))

  # map bw to size in mm using scale_bw funtion
  .data <- .data |>
    dplyr::mutate(
      size_mm = pdr_scale_bw(.tool_width)
    )

  ## Spray and Pen needs to be plotted differently
  ## We achieve this by making a coordinate set for Spray and Pen, respectively

  pdr_pen <- .data |>
    dplyr::filter(.tool == "pen")

  pdr_spray <- .data |>
    dplyr::filter(.tool == "spray")

  ### Recreate jitter in spray Data
  ### But only if spray data exists

  if (nrow(pdr_spray) > 0) {
    pdr_spray <- pdr_spray |>

      # Create extra rows according to spray density (pd)
      tidyr::uncount(weights = .data$.point_density) |>

      # Generate random angles and radii for uniform distribution within a circle
      # pr holds information about the spray radius
      dplyr::mutate(
        angle = runif(dplyr::n(), 0, 2 * pi),
        radius = sqrt(runif(dplyr::n(), 0, 1)) * .spray_radius
      ) |>

      # Calculate offsets and new x, y positions
      dplyr::mutate(
        offsetX = radius * cos(angle),
        offsetY = radius * sin(angle),
        .x = .x + offsetX,
        .y = .y + offsetY
      )
  }

  ## Lets plot!

  ### Base plot
  pdr_base <- pdr_pen |>
    ggplot2::ggplot(ggplot2::aes(
      x = .x,
      y = .y,
      color = .color,
      alpha = .alpha
    ))

  ### If background image is provided, we plot it now
  if (!is.null(background_image)) {
    pdr_base <- pdr_base +
      ggplot2::annotation_raster(
        as.raster(background_image),
        -Inf,
        Inf,
        -Inf,
        Inf
      )
  }

  ### Build plot(s) with pen strokes
  if (type == "polygon") {
    pdr_base <- pdr_base +
      ggplot2::coord_fixed(
        xlim = c(0, image_width),
        ylim = c(0, image_height),
        expand = FALSE
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::scale_color_identity() +
      ggplot2::scale_size_identity() +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_linewidth_identity() +
      ggplot2::geom_polygon(
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = size_mm + 1
        ),
        linetype = 1,
        #linewidth = 0.5,
        color = "black",
        fill = "black",
        alpha = 1
      )
  } else {
    pdr_base <- pdr_base +
      ggplot2::coord_fixed(
        xlim = c(0, image_width),
        ylim = c(0, image_height),
        expand = FALSE
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::scale_color_identity() +
      ggplot2::scale_size_identity() +
      ggplot2::scale_alpha_identity() +
      ggplot2::scale_linewidth_identity() +
      ggplot2::geom_path(
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = size_mm + 1
        ),
        linetype = 1
      )
  }

  plot_out <- pdr_base +
    ggplot2::geom_point(
      data = pdr_spray,
      ggplot2::aes(size = size_mm),
      shape = 15
    )

  plot_out <- plot_out +
    ggplot2::facet_wrap(facets = ~.id)

  if (!include_id) {
    plot_out <- plot_out +
      ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  if (!rasterize) {
    return(plot_out)
  }

  ### Translate size to mm, accounting for facet grid dimensions
  mm_per_inch <- 25.4

  # ncol defaults to what facet_wrap uses: ceiling(sqrt(n))
  n_col <- ceiling(sqrt(id_n))
  n_row <- ceiling(id_n / n_col)

  # Adjust image width and height based on row and column height
  image_width <- image_width * n_col
  image_height <- image_height * n_row

  if (method == "memory") {
    arr <- ragg::agg_capture(
      width = image_width,
      height = image_height,
      units = "px",
      bg = "transparent"
    )
    print(plot_out) # must explicitly print to render to the device
    cap <- arr()
    dev.off()

    # Convert colour names → integer RGBA → numeric [0,1] array
    rgba_int <- col2rgb(cap, alpha = TRUE) # 4 × (w*h) matrix, values 0-255
    rgba_arr <- array(
      t(rgba_int) / 255, # normalise to [0,1]
      dim = c(nrow(cap), ncol(cap), 4)
    )

    # Make transparent pixels black (rather than transparent white)
    rgba_arr[,, 1:3][rgba_arr[,, 4] == 0] <- 0

    raster_img <- as.raster(rgba_arr)
  } else {
    tmp <- tempfile(fileext = ".png")
    if (clean_up) {
      on.exit(unlink(tmp), add = TRUE)
    }

    ggplot2::ggsave(
      plot = plot_out,
      file = tmp,
      width = image_width,
      height = image_height,
      units = "px"
    )

    raster_img <- as.raster(png::readPNG(tmp))
  }

  raster_plot <- ggplot2::ggplot() +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::coord_fixed(ratio = image_height / image_width) +
    ggplot2::annotation_raster(
      raster_img,
      -Inf,
      Inf,
      -Inf,
      Inf,
      interpolate = TRUE
    )

  return(raster_plot)
}
