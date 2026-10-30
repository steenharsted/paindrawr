#' Plot RGBA rasters in a grid
#'
#' Plots the `.rgba` raster arrays stored in a paindrawr list-column as a
#' ggplot, one facet per row of `.data`, using
#' [ggplot2::annotation_raster()]. Optionally draws a background image beneath
#' each raster.
#'
#' @param .data A data frame with a paindrawr list-column (see
#'   `paindrawr_data`) whose elements contain an `.rgba` entry. Add it first
#'   with [pdr_add_rgba()].
#' @param paindrawr_data The list-column in `.data` holding the paindrawr data.
#'   Defaults to `pdr_data`.
#' @param background_image Optional background drawn beneath each raster. Either
#'   a path to a PNG file (character string ending in `.png`) or a numeric
#'   raster array as returned by [png::readPNG()]. If `NULL` (default), no
#'   background is drawn.
#' @param include_id Logical. If `TRUE`, the `.id` value is shown as a facet
#'   strip label above each panel. Default is `FALSE`.
#' @param ncol,nrow Number of columns and rows, passed to
#'   [ggplot2::facet_wrap()]. If both are `NULL` (default), `facet_wrap()`
#'   chooses the layout.
#'
#' @return A [ggplot2::ggplot()] object with one facet per drawing.
#'
#' @examples
#' \dontrun{
#' pdr_example_data[1,] |>
#'   mutate(pdr_data = pdr_add_rgba(pdr_data)) |>
#'   pdr_plot_rgba()
#'
#' pdr_example_data[1:10,] |>
#'   mutate(pdr_data = pdr_add_rgba(pdr_data)) |>
#'   pdr_plot_rgba(
#'     background_image = pdr_example_file("mird_body_background.png"),
#'     include_id = TRUE,
#'     ncol = 2
#'   )
#' }
#'
#' @export
pdr_plot_rgba <- function(
  .data,
  paindrawr_data = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  ncol = NULL,
  nrow = NULL
) {
  # Validate and read background_image (NULL, PNG path, or raster array)
  background_image <- read_background_image(background_image)

  .data <- .data |>
    dplyr::select({{ paindrawr_data }}) |>
    tidyr::unnest_wider({{ paindrawr_data }})

  if (!".rgba" %in% names(.data)) {
    stop("No .rgba in data, run pdr_add_rgba() first")
  }

  ids <- if (".id" %in% names(.data)) {
    as.character(.data$.id)
  } else {
    as.character(seq_len(nrow(.data)))
  }
  rgbas <- .data$.rgba
  keep <- !vapply(rgbas, function(x) is.null(x) || length(x) == 0, logical(1))
  if (!any(keep)) {
    stop("No non-missing .rgba to plot")
  }
  ids <- ids[keep]
  rgbas <- rgbas[keep]

  # Canvas size in pixels from the first raster
  first_dim <- dim(rgbas[[1]])
  width <- first_dim[2]
  height <- first_dim[1]

  # annotation_raster() has no per-facet data, so add one layer per panel,
  # restricted to its facet via a data frame holding that panel's .id
  raster_layer <- function(img, id) {
    ggplot2::layer(
      geom = ggplot2::GeomRasterAnn,
      stat = ggplot2::StatIdentity,
      data = data.frame(.id = id),
      mapping = ggplot2::aes(),
      position = ggplot2::PositionIdentity,
      inherit.aes = FALSE,
      show.legend = FALSE,
      params = list(
        raster = as.raster(img),
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        interpolate = TRUE
      )
    )
  }

  plot_out <- ggplot2::ggplot(data.frame(.id = ids))

  if (!is.null(background_image)) {
    for (id in ids) {
      plot_out <- plot_out + raster_layer(background_image, id)
    }
  }
  for (i in seq_along(ids)) {
    plot_out <- plot_out + raster_layer(rgbas[[i]], ids[i])
  }

  plot_out <- plot_out +
    ggplot2::coord_fixed(
      xlim = c(0, width),
      ylim = c(0, height),
      expand = FALSE
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::facet_wrap(facets = ~.id, nrow = nrow, ncol = ncol)

  if (!include_id) {
    plot_out <- plot_out +
      ggplot2::theme(strip.text = ggplot2::element_blank())
  }

  plot_out
}
