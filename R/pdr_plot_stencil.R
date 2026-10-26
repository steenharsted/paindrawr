#' Plot stencil data
#'
#' The function is primarily for testing purposes -- it 
#' plots the .stencil matrix data (or .stencil_inv for the 
#' inverted stencil).
#' 
#' It functions basically as a wrapper for the 
#' `pdr_plot_rgba()` function.
#'
#' @param .data A data frame with a pdr_list column (see
#'   `paindrawr_data`) whose elements contains `.stencil` 
#'   and `.stencil_inv` data. These are added by the 
#'   [pdr_add_stencil()] function.
#' @param paindrawr_data The pdr_list column in `.data` 
#'   which hold the pain drawing data. 
#'   Defaults to `pdr_data`.
#' @param background_image Optional background drawn beneath 
#'   each raster. Either a path to a PNG file (character 
#'   string ending in `.png`) or a numeric raster array as 
#'   returned by [png::readPNG()]. If `NULL` (default), no
#'   background is drawn.
#' @param ncol,nrow Number of columns and rows in the plot grid. If both are
#'   `NULL` (default), [gridExtra::grid.arrange()] chooses the layout.
#' @param invert If TRUE, the inverted stencil is plotted.
#'
#' @returns A ggplot of the .stencil 
#'
#' @export
#' @examples#'
#' pdr_example_anatomy[1,] |> pdr_plot_stencil(pdr_data, invert=TRUE)
#' 
pdr_plot_stencil <- function(.data, pdr_list, background_image=NULL, ncol = NULL, nrow = NULL, invert=FALSE) {

  if(!pdr_match(.data, as="pdr_tibble")) { stop("Invalid '.data' parameter - should be pdr_tibble.")}

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
    mutate( {{pdr_list}} := {{pdr_list}} |>
      purrr::map(\(pdr_drawing) {
        pdr_drawing$.rgba[,,1] <- 1
        pdr_drawing$.rgba[,,2] <- 0
        pdr_drawing$.rgba[,,3] <- 0
        if(invert) {
          pdr_drawing$.rgba[,,4] <- pdr_drawing$.stencil_inv
        } else {
          pdr_drawing$.rgba[,,4] <- pdr_drawing$.stencil
        }
        pdr_drawing
      })) |> pdr_plot_rgba({{pdr_list}}, background_image, ncol, nrow)


}
