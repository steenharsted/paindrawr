#' Plot stencil data
#'
#' Plots the `.stencil` matrix of each drawing (or `.stencil_inverted` for the
#' inverted stencil) as a red mask. The function is primarily for checking
#' stencils created with [pdr_add_stencil()].
#'
#' It is a wrapper for [pdr_plot_rgba()]: the `.rgba` of each drawing is
#' replaced by a red raster whose alpha channel is the stencil, and the result
#' is passed on to [pdr_plot_rgba()]. The drawings must therefore already have
#' an `.rgba` element (see [pdr_add_rgba()]).
#'
#' @inheritParams pdr_plot_rgba
#' @param .data A pdr_tibble (see [pdr_match()]) whose pdr_drawings contain
#'   `.rgba`, `.stencil` and `.stencil_inverted` elements. The stencil
#'   elements are added by [pdr_add_stencil()].
#' @param ncol,nrow Number of columns and rows in the plot grid, passed to
#'   [ggplot2::facet_wrap()] via [pdr_plot_rgba()]. If both are `NULL`
#'   (default), `facet_wrap()` chooses the layout.
#' @param invert Logical. If `TRUE`, the inverted stencil
#'   (`.stencil_inverted`) is plotted instead of `.stencil`. Default is
#'   `FALSE`.
#'
#' @returns A [ggplot2::ggplot()] object with one facet per drawing, showing
#'   the stencil.
#'
#' @seealso [pdr_add_stencil()], [pdr_plot_rgba()]
#'
#' @export
#' @examples
#' \donttest{
#' pdr_stencil_full_body[1:2, ] |>
#'   pdr_plot_stencil(pdr_data)
#'
#' pdr_stencil_full_body[1:2, ] |>
#'   pdr_plot_stencil(
#'     pdr_data,
#'     background_image = pdr_example_file("bg_full_body.png"),
#'     invert = TRUE
#'   )
#' }
pdr_plot_stencil <- function(
  .data,
  pdr_col = pdr_data,
  background_image = NULL,
  ncol = NULL,
  nrow = NULL,
  invert = FALSE
) {
  if (!isTRUE(pdr_match(.data, as = "pdr_tibble"))) {
    stop("Invalid '.data' parameter - should be pdr_tibble.")
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

  # Replace the .rgba element with one where all pixels are
  # full red, but the alpha channel is the stencil matrix
  # (or the inverted stencil) .. and plot as pdr_plot_rgba
  .data |>
    mutate(
      {{ pdr_col }} := {{ pdr_col }} |>
        purrr::map(\(pdr_drawing) {
          pdr_drawing$.rgba[,, 1] <- 1
          pdr_drawing$.rgba[,, 2] <- 0
          pdr_drawing$.rgba[,, 3] <- 0
          if (invert) {
            pdr_drawing$.rgba[,, 4] <- pdr_drawing$.stencil_inverted
          } else {
            pdr_drawing$.rgba[,, 4] <- pdr_drawing$.stencil
          }
          pdr_drawing
        })
    ) |>
    pdr_plot_rgba(
      {{ pdr_col }},
      background_image = background_image,
      ncol = ncol,
      nrow = nrow
    )
}
