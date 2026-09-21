#' Plot polygons
#'
#' This function is a simple wrapper for the
#' `pdr_plot_drawing()` function.
#'
#' It substitutes the polygon coordinates for the raw data
#' coordinates in the pain drawing data list-col, before
#' plotting.
#'
#' For a description of parameters and other details,
#' see `pdr_plot_drawing()` documentation.
#'
#' @export
#'
#' @examples
#' # Plot the raw data from geometry examples
#' pdr_example_geometry |> pdr_plot_drawing()
#'
#' # Plot as polygons -- note closure and buffering
#' pdr_example_geometry |>
#'   mutate(pdr_data = pdr_polygonize(pdr_data, buffer=5)) |>
#'   pdr_plot_polygons()
#'
#' # Merge overlapping strokes before plotting polygons
#' pdr_example_geometry |>
#'   mutate(pdr_data = pdr_polygonize(pdr_data, buffer=5)) |>
#'   mutate(pdr_data = pdr_modify_polygons(pdr_data, "merge_overlaps")) |>
#'   pdr_plot_polygons()

pdr_plot_polygons <- function(
  .data,
  pdr_list = pdr_data,
  background_image = NULL,
  include_id = FALSE
) {
  
  #### From SHFs plot drawing function:
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


  #### This is temporary -- how to deal with multi-plots?
  dd <- .data |> dplyr::pull({{pdr_list}})
  dd <- dd[[1]]

  
  gp <- ggplot2::ggplot(data.frame(x=c(0, dd$.width), y = c(dd$.height,0)), aes(x, y)) + ggplot2::geom_point()

  if (!is.null(background_image)) {
    gp <- gp <- ggplot2::ggplot() + 
      ggplot2::annotation_raster(
        as.raster(background_image),
        -Inf,
        Inf,
        -Inf,
        Inf
      )
  }

  gp <- gp + ggplot2::geom_sf(data = dd$.polygons, alpha=0.5)

  gp
  
}
