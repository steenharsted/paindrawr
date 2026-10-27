pdr_plot_polygons <- function(
  pdr_tib,
  pdr_list = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  multi = "facet"
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


  dd <- pdr_tib |> dplyr::pull({{pdr_list}})
  
  width <- dd |> purrr::map_int(\(pdr_drawing) {pdr_drawing$width}) |> max()
  height <- dd |> purrr::map_int(\(pdr_drawing) {pdr_drawing$height}) |> max()

  dd <- dd |> pdr_get_info()

  if(!is.null(background_image)) {
    gp <- ggplot() +
      ggplot2::scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, height), expand = c(0, 0)) +
      ggplot2::annotation_raster(
      as.raster(background_image),
      0, width, 0, height
    )
  } else {
    gp <- ggplot() +
      ggplot2::scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, height), expand = c(0, 0))
  }

  if(identical(NA, dd$.polygons)) {
    gp 
  } else {    
    if(multi=="facet") {
      gp + ggplot2::geom_sf(data = dd$.polygons, color="black", fill="white", alpha=0.6) + facet_wrap(~dd$.id)
    } else {
      gp + ggplot2::geom_sf(data = dd$.polygons, color=forcats::as_factor(dd$.id), fill="white", alpha=0.6)
    }
  }
  
}
