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
#' @inheritParams pdr_plot_rgba
#' @param rasterize Logical. If `TRUE` (default), the plot is rasterized and
#'   returned as a new ggplot with [ggplot2::annotation_raster()]. The output
#'   is pixel-accurate with a locked aspect ratio. If `FALSE`, the raw ggplot
#'   with vector strokes is returned.
#' @inheritParams pdr_add_rgba
#' @returns A [ggplot2::ggplot()] object. When `rasterize = TRUE`, the plot
#'   contains a single [ggplot2::annotation_raster()] layer with a locked
#'   aspect ratio. When `rasterize = FALSE`, the plot contains vector stroke
#'   layers and supports further ggplot2 additions.
#'
#' @seealso [pdr_import_json()] to read pain drawing JSON files,
#'   [pdr_plot_rgba()] to plot rasterised drawings, [pdr_add_rgba()] to store
#'   raster arrays in a pdr_list, [pdr_match()] for the pdr_tibble definition.
#'
#' @examples
#' bg <- pdr_example_file("bg_full_body.png")
#' pd <- pdr_example_data[1:2, ]
#'
#' # Default: in-memory rasterization with a background image
#' pdr_plot_xy(pd, background_image = bg)
#'
#' \donttest{
#' # File-based rasterization
#' pdr_plot_xy(pd, method = "file")
#'
#' # Vector output -- can add further ggplot layers
#' pdr_plot_xy(pd, rasterize = FALSE)
#'
#' # Show id labels in facet strips
#' pdr_plot_xy(pd, include_id = TRUE)
#'
#' # Draw strokes as filled polygons
#' pdr_plot_xy(pd, polygon = "both")
#'
#' # Use a custom list-column name
#' pd2 <- dplyr::rename(pd, my_col = pdr_data)
#' pdr_plot_xy(pd2, pdr_col = my_col)
#' }
#'
#' @importFrom dplyr select mutate filter full_join join_by n
#' @importFrom tidyr unnest unnest_wider uncount
#' @importFrom png readPNG
#' @importFrom ragg agg_capture
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void theme element_blank scale_color_identity scale_size_identity scale_alpha_identity scale_linewidth_identity geom_path geom_tile facet_wrap annotation_raster ggsave
#' @importFrom cli cli_warn cli_abort
#'
#' @export
pdr_plot_xy <- function(
  .data,
  pdr_col = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  rasterize = TRUE,
  method = "memory",
  clean_up = TRUE,
  polygon = "none"
) {
  # Drop missing drawings (NA or NULL entries) before unnesting
  drawings <- dplyr::pull(.data, {{ pdr_col }})
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
  .data <- .data |> tidyr::unnest_wider({{ pdr_col }})

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

  background_image <- read_background_image(background_image)

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

  # Rendering resolution. The value is arbitrary and only affects text size
  # (facet labels); it must be the same in the linewidth formula below and in
  # both rasterization calls (agg_capture, ggsave), or pen widths will be wrong.
  render_dpi <- 72

  # ggplot2 linewidth (mm) giving a line .tool_width canvas pixels wide at
  # render_dpi (1 lwd unit = 1/96 inch, 1 linewidth unit = .pt lwd units)
  .data <- .data |>
    dplyr::mutate(
      lw = .tool_width * 96 / (render_dpi * ggplot2::.pt)
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

      # Random angles and radii. The radius is uniform (not sqrt-transformed)
      # to match the PainDraw module, so dots cluster towards the centre
      dplyr::mutate(
        angle = runif(dplyr::n(), 0, 2 * pi),
        radius = runif(dplyr::n(), 0, 1) * .spray_radius
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
      fill = .color,
      alpha = .alpha
    )) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_linewidth_identity()

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
  if (polygon %in% c("pen", "both")) {
    pdr_base <- pdr_base +
      ggplot2::coord_fixed(
        xlim = c(0, image_width),
        ylim = c(0, image_height),
        expand = FALSE
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = lw
        ),
        linetype = 1,
        alpha = 1
      )
  } else {
    pdr_base <- pdr_base +
      ggplot2::coord_fixed(
        xlim = c(0, image_width),
        ylim = c(0, image_height),
        expand = FALSE
      ) +
      ggplot2::geom_path(
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = lw
        ),
        linetype = 1,
        lineend = "round",
        linejoin = "round"
      )
  }

  # Draw spray or spray polygon
  if (polygon %in% c("spray", "both")) {
    plot_out <- pdr_base +
      ggplot2::geom_polygon(
        data = pdr_spray,
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = lw
        ),
        alpha = 1,
        linetype = 1
      )
  } else {
    plot_out <- pdr_base +
      # Each dot is a square with side .tool_width canvas pixels, as in the module
      ggplot2::geom_tile(
        data = pdr_spray,
        ggplot2::aes(width = .tool_width, height = .tool_width),
        color = NA
      )
  }

  plot_out <- plot_out +
    ggplot2::facet_wrap(facets = ~.id)

  if (!include_id) {
    plot_out <- plot_out +
      ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  if (!rasterize) {
    return(plot_out)
  }

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
      res = render_dpi,
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
      units = "px",
      dpi = render_dpi
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
