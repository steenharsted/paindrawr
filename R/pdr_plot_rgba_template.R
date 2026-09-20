#' Plot an RGBA template
#'
#' The function is primarily for testing purposes -- it plots a matrix of RGBA values
#'
#' @param pdr_rgba A 2-dimensional matrix corresponding to the canvas size (width x height) with values of 0 or 1
#'
#' @returns A ggplot of the matrix where 1 values are 100% opague and 0 values are 100% transparent
#'
#' @export
#' @examples#'
pdr_plot_rgba_template <- function(.data, pdr_rgba, background_image=NULL) {

  gplt <- .data |> dplyr::pull({{ pdr_rgba }}) |>
    purrr::map(~reshape2::melt(.x) |> dplyr::rename(y=Var1, x=Var2)) |>
    purrr::map_dfr(~dplyr::mutate(.x, id=dplyr::cur_group_id())) |>
    ggplot2::ggplot(aes(x=x, y=y, alpha=value)) +
      ggplot2::geom_raster() +
      ggplot2::facet_wrap(~id) 


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


  # if (!is.null(background_image)) {
  #   gplt <- gplot +
  #     ggplot2::annotation_raster(
  #       as.raster(background_image),
  #       -Inf,
  #       Inf,
  #       -Inf,
  #       Inf
  #     )
  # }

  gplt # return a plot
}
