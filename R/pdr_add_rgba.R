#' Add an RGBA raster array to a pain drawing
#'
#' Renders the strokes and spray points from one or more pain drawings into
#' RGBA numeric arrays. By default, the array is attached as a `$.rgba` element
#' to each drawing and the augmented drawing(s) are returned. Set
#' `rgba_only = TRUE` to return the bare array(s) instead, which is useful
#' when assigning into a tibble list-column directly.
#'
#' @param paindrawr_data A pain drawing in one of two forms:
#'   * A single named list as produced by [pdr_import_json()] or extracted
#'     from a list-column (e.g. `pd$pdr_data[[1]]`).
#'   * A list-of-named-lists containing multiple drawings (e.g.
#'     `pdr_example_data`).
#' @param method Character. Controls the rasterization back-end. Either:
#'   * `"memory"` (default) — renders in-memory via [ragg::agg_capture()],
#'     no temp file is written to disk.
#'   * `"file"` — saves to a temporary PNG via [ggplot2::ggsave()] and reads
#'     it back with [png::readPNG()]. Use this if the `"memory"` path is slow.
#' @param clean_up Logical. If `TRUE` (default), the temporary PNG file is
#'   deleted on exit. Only relevant when `method = "file"`. Set to `FALSE` to
#'   retain the file for debugging.
#' @param dpi Resolution used during rasterization. Defaults to `96`. Increase for print-quality output (e.g. `300`), noting this
#'   does not affect the canvas dimensions, only output pixel density.
#' @return A named list per drawing (or a list of such lists when multiple
#'   drawings are supplied):
#'   * `rgba_only = FALSE` (default): the input drawing with `$.rgba` appended.
#'   * `rgba_only = TRUE`: a named list with a single `$.rgba` element.
#'
#'   The `$.rgba` element is a numeric array of dimensions
#'   `height × width × 4` (RGBA channels, values in \[0, 1\]). Transparent
#'   pixels are set to black (`RGB = 0`).
#'
#' @return
#'   * `rgba_only = FALSE` (default): the input drawing (named list) with
#'     `$.rgba` appended, or a list of such augmented drawings when multiple
#'     drawings are supplied.
#'   * `rgba_only = TRUE`: a numeric array of dimensions
#'     `height × width × 4` (RGBA channels, values in \[0, 1\]), or a list of
#'     such arrays when multiple drawings are supplied. Transparent pixels are
#'     set to black (`RGB = 0`).
#'
#' @examples
#' \dontrun{
#' # Single drawing — returns drawing with $.rgba appended
#' drawing_with_rgba <- pdr_example_data[[1]] |> pdr_add_rgba()
#' grid::grid.newpage()
#' grid::grid.raster(drawing_with_rgba$.rgba)
#'
#' # Multiple drawings at once
#' drawings_with_rgba <- pdr_example_data |> pdr_add_rgba()
#'
#' # Tibble workflow
#'
#' ## Data
#' pd <- tibble(pdr_data = pdr_example_data)
#'
#' ## add .rgba to the input list-column
#' pd |> dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
#'
#' ## add rgba arrays in a separate list-column
#' pd |>
#'   dplyr::mutate(
#'     rgba_arrays = pdr_add_rgba(pdr_data, rgba_only = TRUE)
#'     )
#' }
#'
#' @importFrom dplyr select mutate filter full_join n pull join_by
#' @importFrom tidyr unnest unnest_wider uncount
#' @importFrom tibble tibble
#' @importFrom png readPNG
#' @importFrom ragg agg_capture
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void scale_color_identity
#' @importFrom ggplot2 scale_size_identity scale_alpha_identity scale_linewidth_identity
#' @importFrom ggplot2 geom_path geom_point ggsave
#' @importFrom purrr map
#' @importFrom cli cli_abort
#'
#' @export
pdr_add_rgba <- function(
  paindrawr_data,
  method = "memory",
  clean_up = TRUE,
  dpi = 96,
  rgba_only = FALSE
) {
  # Detect whether we were handed a single bare drawing (vs a list-of-drawings).
  single_input <- is.list(paindrawr_data) && ".id" %in% names(paindrawr_data)
  if (single_input) {
    paindrawr_data <- list(paindrawr_data)
  }

  input_data <- paindrawr_data[[1]] # single drawing, preserved for re-attaching .rgba

  # Always process element-wise and return a list-of-results. This keeps the
  # return type consistent for tibble list-columns (one list element per row).
  # A bare single-drawing input is unwrapped again at the end.
  if (length(paindrawr_data) > 1) {
    return(
      purrr::map(
        paindrawr_data,
        \(x) {
          pdr_add_rgba(
            paindrawr_data = x,
            method = method,
            clean_up = clean_up,
            dpi = dpi,
            rgba_only = rgba_only
          )
        }
      )
    )
  }

  method <- match.arg(method, choices = c("memory", "file"))

  .data <- tibble::tibble(
    .list_col = paindrawr_data
  ) |>
    tidyr::unnest_wider(col = .list_col)

  # Extract height and width from the single row
  image_width <- .data |> dplyr::pull(.width) |> unique()
  image_height <- .data |> dplyr::pull(.height) |> unique()

  # Unnest and join the s (strokes) and p (points) list-columns
  pdr_s <- .data |>
    dplyr::select(.id, .strokes) |>
    tidyr::unnest(cols = .strokes)

  pdr_p <- .data |>
    dplyr::select(.id, .points) |>
    tidyr::unnest(cols = .points)

  pd <- dplyr::full_join(pdr_s, pdr_p, by = dplyr::join_by(.id, .index)) # ".index" is the stroke-index-column that comes from unnesting .strokes and .points

  # Scale brush width (pixels) to mm for ggplot rendering
  pd <- pd |>
    dplyr::mutate(size_mm = pdr_scale_bw(.tool_width))

  # Split into pen and spray subsets
  pdr_pen <- dplyr::filter(pd, .tool == "pen")
  pdr_spray <- dplyr::filter(pd, .tool == "spray")

  # Recreate spray jitter — only when spray strokes exist
  if (nrow(pdr_spray) > 0) {
    pdr_spray <- pdr_spray |>
      tidyr::uncount(weights = .data$.point_density) |>

      # Uniform distribution within a circle of radius pr
      dplyr::mutate(
        angle = runif(dplyr::n(), 0, 2 * pi),
        radius = sqrt(runif(dplyr::n(), 0, 1)) * .spray_radius
      ) |>
      dplyr::mutate(
        .x = .x + radius * cos(angle),
        .y = .y + radius * sin(angle)
      )
  }

  # Base plot (pen strokes)
  pdr_base <- pdr_pen |>
    ggplot2::ggplot(ggplot2::aes(
      x = .x,
      y = .y,
      color = .color,
      alpha = .alpha
    )) +
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

  # Add spray points on top
  plot_out <- pdr_base +
    ggplot2::geom_point(
      data = pdr_spray,
      ggplot2::aes(size = size_mm),
      shape = 15
    )

  # Translate canvas pixels to mm for the rendering device
  mm_per_inch <- 25.4
  pixel_to_mm <- mm_per_inch / 96 # physical canvas size is fixed at 96 dpi
  width_mm <- image_width * pixel_to_mm
  height_mm <- image_height * pixel_to_mm

  if (method == "memory") {
    arr <- ragg::agg_capture(
      width = width_mm,
      height = height_mm,
      units = "mm",
      res = dpi,
      bg = "transparent"
    )
    suppressMessages(print(plot_out)) # must explicitly print to render to the device
    cap <- arr()
    dev.off()

    # Convert colour names → integer RGBA → numeric [0,1] array
    rgba_int <- col2rgb(cap, alpha = TRUE) # 4 × (w*h) matrix, values 0-255
    rgba_arr <- array(
      t(rgba_int) / 255, # normalise to [0,1]
      dim = c(image_height, image_width, 4) # height × width × RGBA
    )
  } else {
    tmp <- tempfile(fileext = ".png")
    if (clean_up) {
      on.exit(unlink(tmp), add = TRUE)
    }

    suppressMessages(
      ggplot2::ggsave(
        plot = plot_out,
        file = tmp,
        width = width_mm,
        height = height_mm,
        units = "mm",
        dpi = dpi
      )
    )

    rgba_arr <- png::readPNG(tmp)
  }

  # Make transparent pixels black (rather than transparent white)
  rgba_arr[,, 1:3][rgba_arr[,, 4] == 0] <- 0

  if (rgba_only) {
    result <- list(.rgba = rgba_arr)
  } else {
    input_data$.rgba <- rgba_arr
    result <- input_data
  }

  # Bare single-drawing input → return the result directly.
  # Any other input (incl. a length-1 list-column) → return a list, so each
  # tibble row receives one list element.
  if (single_input) {
    result
  } else {
    list(result)
  }
}
