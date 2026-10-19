#' Plot RGBA rasters in a grid
#'
#' Plots the `.rgba` raster arrays stored in a paindrawr list-column, one
#' panel per row of `.data`. Optionally draws a background image beneath each
#' raster.
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
#' @param ncol,nrow Number of columns and rows in the plot grid. If both are
#'   `NULL` (default), [gridExtra::grid.arrange()] chooses the layout.
#'
#' @return Called for its side effect of drawing the plot. Invisibly returns
#'   the arranged gtable from [gridExtra::grid.arrange()].
#'
#' @examples
#' \dontrun{
#' my_data |>
#'   pdr_plot_rgba(palmar)
#'
#' my_data |>
#'   pdr_plot_rgba(
#'     palmar,
#'     background_image = "inst/extdata/lower_arm_palmar.png",
#'     ncol = 2
#'   )
#' }
#'
#' @export
pdr_plot_rgba <- function(
  .data,
  paindrawr_data = pdr_data,
  background_image = NULL,
  ncol = NULL,
  nrow = NULL
) {
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

  # pdr_list
  .data <- .data |>
    dplyr::select({{ paindrawr_data }}) |>
    tidyr::unnest_wider({{ paindrawr_data }})

  # check .rgba exists
  if (!".rgba" %in% names(.data)) {
    stop("No .rgba in data, run pdr_add_rgba() first")
  }

  # Make into a list of rgbas
  .data <- .data |> dplyr::pull(.rgba)

  # One grob per element: background underneath, rgba on top
  plots <- lapply(.data, function(rgba) {
    if (is.null(rgba) || length(rgba) == 0) {
      return(grid::nullGrob())
    }
    fg <- grid::rasterGrob(
      rgba
    )

    if (is.null(background_image)) {
      return(fg)
    }
    bg_grob <- grid::rasterGrob(
      background_image
    )
    grid::gTree(children = grid::gList(bg_grob, fg))
  })

  gridExtra::grid.arrange(
    grobs = plots,
    ncol = ncol,
    nrow = nrow
  )
}
