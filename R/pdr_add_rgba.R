#' Add an RGBA raster array to a pain drawing based on spray coordinates
#'
#' Renders the strokes and spray points from one or more pain drawings into
#' RGBA numeric arrays. By default, the array is attached as a `$.rgba` element
#' to each drawing and the augmented drawing(s) are returned. Set
#' `rgba_only = TRUE` to return only the array(s), each wrapped in a named
#' list with a single `$.rgba` element. This is useful when assigning into a
#' separate tibble list-column.
#'
#' The output always has the same shape as the input: a single drawing returns
#' a single result, and a list of drawings (of any length, including 1)
#' returns a list of results of the same length.
#'
#' @param paindrawr_data A pain drawing in one of two forms:
#'   * A single drawing: a named list containing a `.strokes` element, as
#'     produced by [pdr_import_json()] or extracted from a list-column
#'     (e.g. `pd$pdr_data[[1]]`).
#'   * A list of drawings of any length, including 1 (e.g.
#'     `pdr_example_data` or a list-column such as `pd$pdr_data`).
#' @param method Character. Controls the rasterization back-end. Either:
#'   * `"memory"` (default) — renders in-memory via [ragg::agg_capture()],
#'     no temp file is written to disk.
#'   * `"file"` — saves to a temporary PNG via [ggplot2::ggsave()] and reads
#'     it back with [png::readPNG()]. Use this if the `"memory"` path is slow.
#' @param clean_up Logical. If `TRUE` (default), the temporary PNG file is
#'   deleted on exit. Only relevant when `method = "file"`. Set to `FALSE` to
#'   retain the file for debugging.
#' @param polygon Character. Controls whether strokes are rendered as filled
#'   polygons (in black) instead of their original appearance. One of:
#'   * `"none"` (default) — pen strokes as paths, spray strokes as points.
#'   * `"pen"` — pen strokes as filled polygons.
#'   * `"spray"` — spray strokes as filled polygons.
#'   * `"both"` — both pen and spray strokes as filled polygons.
#' @param rgba_only Logical. If `FALSE` (default), returns the input
#'   drawing(s) with a `$.rgba` element appended to each. If `TRUE`, returns
#'   a named list with a single `$.rgba` element (single drawing) or a list
#'   of such named lists (list of drawings). Use `TRUE` when assigning into
#'   a separate tibble list-column:
#'   `mutate(rgba = pdr_add_rgba(pdr_data, rgba_only = TRUE))`.
#'
#' @return
#'   * `rgba_only = FALSE` (default): the input drawing (named list) with
#'     `$.rgba` appended, or a list of such augmented drawings when a list of
#'     drawings is supplied.
#'   * `rgba_only = TRUE`: a named list with a single element `$.rgba`, or a
#'     list of such named lists when a list of drawings is supplied.
#'
#'   In both cases `$.rgba` is a numeric array of dimensions
#'   `height × width × 4` (RGBA channels, values in \[0, 1\]). Transparent
#'   pixels are set to black (`RGB = 0`).
#'
#' @examples
#' \dontrun{
#' # Single drawing — returns drawing with $.rgba appended
#' drawing_with_rgba <- pdr_example_data[[1]] |> pdr_add_rgba()
#' grid::grid.newpage()
#' grid::grid.raster(drawing_with_rgba$.rgba)
#'
#' # Single drawing — returns list(.rgba = <array>)
#' rgba <- pdr_example_data[[1]] |> pdr_add_rgba(rgba_only = TRUE)
#' dim(rgba$.rgba)
#'
#' # Multiple drawings at once
#' drawings_with_rgba <- pdr_example_data |> pdr_add_rgba()
#'
#' # Render spray strokes as filled polygons
#' pdr_example_data[[1]] |> pdr_add_rgba(polygon = "spray")
#'
#' # Tibble workflow
#'
#' ## Data
#' pd <- tibble::tibble(pdr_data = pdr_example_data)
#'
#' ## add .rgba to the input list-column
#' pd |> dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
#'
#' ## add rgba arrays in a separate list-column
#' pd_rgba <- pd |>
#'   dplyr::mutate(
#'     rgba = pdr_add_rgba(pdr_data, rgba_only = TRUE)
#'   )
#'
#' ## access the array for the first row
#' pd_rgba$rgba[[1]]$.rgba
#' }
#'
#' @importFrom dplyr select mutate filter full_join n pull join_by
#' @importFrom tidyr unnest unnest_wider uncount
#' @importFrom tibble tibble
#' @importFrom png readPNG
#' @importFrom ragg agg_capture
#' @importFrom ggplot2 ggplot aes coord_fixed theme_void scale_color_identity
#' @importFrom ggplot2 scale_size_identity scale_alpha_identity scale_linewidth_identity
#' @importFrom ggplot2 geom_path geom_point geom_polygon ggsave
#' @importFrom purrr map
#' @importFrom cli cli_abort
#' @importFrom stats runif
#' @importFrom grDevices col2rgb dev.off
#'
#' @export
pdr_add_rgba <- function(
  paindrawr_data,
  method = "memory",
  clean_up = TRUE,
  polygon = "none",
  rgba_only = FALSE
) {
  # Check arguments
  method <- match.arg(method, choices = c("memory", "file"))
  polygon <- match.arg(polygon, choices = c("none", "pen", "spray", "both"))

  # A single drawing is a named list containing `.strokes`;
  # anything else is treated as a list of drawings (of any length, incl. 1)
  is_single_drawing <- is.list(paindrawr_data) &&
    ".strokes" %in% names(paindrawr_data)

  if (!is_single_drawing) {
    return(
      purrr::map(
        paindrawr_data,
        \(x) {
          pdr_add_rgba(
            paindrawr_data = x,
            polygon = polygon,
            method = method,
            clean_up = clean_up,
            rgba_only = rgba_only
          )
        }
      )
    )
  }

  .data <- tibble::tibble(
    .list_col = list(paindrawr_data)
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

  # Base plot
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
    ggplot2::scale_linewidth_identity()

  # Draw pen or pen polygon
  if (polygon %in% c("pen", "both")) {
    pdr_base <- pdr_base +
      ggplot2::geom_polygon(
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = size_mm + 1
        ),
        linetype = 1,
        color = "black",
        fill = "black"
      )
  } else {
    pdr_base <- pdr_base +
      ggplot2::geom_path(
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = size_mm + 1
        ),
        linetype = 1
      )
  }

  # Draw spray or spray polygon
  if (polygon %in% c("spray", "both")) {
    plot_out <- pdr_base +
      ggplot2::geom_polygon(
        data = pdr_spray,
        ggplot2::aes(
          group = paste0(.id, "_", .index),
          linewidth = size_mm + 1
        ),
        linetype = 1,
        color = "black",
        fill = "black"
      )
  } else {
    plot_out <- pdr_base +
      ggplot2::geom_point(
        data = pdr_spray,
        ggplot2::aes(size = size_mm),
        shape = 15
      )
  }

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
      dim = c(image_height, image_width, 4) # height × width × RGBA
    )
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

    rgba_arr <- png::readPNG(tmp)
  }

  # Make transparent pixels black (rather than transparent white)
  rgba_arr[,, 1:3][rgba_arr[,, 4] == 0] <- 0

  if (rgba_only) {
    return(list(.rgba = rgba_arr))
  } else {
    paindrawr_data$.rgba <- rgba_arr
    return(paindrawr_data)
  }
}
