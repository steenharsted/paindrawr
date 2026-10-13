#' Create a new named (.rgba) element in the pdr list column
#' 
#' The function handles the rgba matrix calculation in three
#' distinctly different ways, as determined by the .tool 
#' value in the pain drawing data or as specified by the 
#' tool_override parameter.
#' 
#' The original drawing can be re-created with tools 'spray' 
#' and 'pen' as recorded in the data (or with tool_override). 
#' 
#' If tool_override is set to 'polygon', each drawing stroke 
#' will be handled as drawn by a pen tool, but strokes will 
#' be coerced into filled-in geometric polygons.
#' 
#' If pen_width_override is set to NULL (default) the pen 
#' width will be set as recorded in the data.
#'
#' @param pdr_list A valid pain drawing pdr_list
#' @param tool_width_override A numeric value specifying pen width
#' @param tool_override A string of NULL, 'pen', 'spray' or 'polygon'
#'
#' @returns A modified pdr_list with .rgba elements added
#'
#' @export
#' @examples
#' 
pdr_add_rgba <- function(pdr_list, tool_width_override = NULL, tool_override = NULL) { #, method="memory", dpi=96, clean_up=TRUE, rgba_only=TRUE, invert = FALSE) {
  ##### Sanity checks
  tool_override <- match.arg(tool_override, c(NULL, "spray", "pen", "polygon"))

  ##### Local helper function
  build_geom <- function(xy, tool, tool_width) {
    if(tool == "pen") {
      return(
        ggplot2::geom_path(
          data = xy,
          linewidth = tool_width,
          linetype = 1,
          color="black"
        )
      )
    }

    if(tool == "spray") {
      #...
    }

    if(tool == "polygon") {
      return(
        ggplot2::geom_polygon(
          data = xy,
          linewidth = 0.5*tool_width,
          linetype = 1,
          color="black",
          fill="black"
        )
      )
    }    
  }

  # Iterate the list and generate rgba data for each drawing
  pdr_list <- pdr_list |> purrr::map(\(pdr_drawing) {
    if(identical(NA,pdr_drawing$.points) || is.null(pdr_drawing$.points) || nrow(pdr_drawing$.points)==0) {
      pdr_drawing$.rgba <- NA 
      pdr_drawing$.ggplot <- NA
    } else {
      # Create an empty base plot
      gp <- ggplot2::ggplot(data = tibble::tibble(.x = as.integer(), .y=as.integer(), .index=as.integer()), #pdr_drawing$.points, 
              mapping = ggplot2::aes(x=.x, y=.y)) +
              ggplot2::coord_fixed(
                xlim = c(0, pdr_drawing$.width),
                ylim = c(0, pdr_drawing$.height),
                expand = FALSE) +
              ggplot2::theme_void(base_size = 12) +
              ggplot2::scale_color_identity() +
              ggplot2::scale_size_identity() +
              ggplot2::scale_alpha_identity() +
              ggplot2::scale_linewidth_identity()

      # Deal with tool override (users input):
      if(!is.null(tool_override)) {pdr_drawing$.strokes$.tool <- tool_override}

      # Deal with pen_width override (users input):
      if(!is.null(tool_width_override)) {pdr_drawing$.strokes$.tool_width <- tool_width_override}

      # Now iterate over each stroke in data
      for(i in unique(pdr_drawing$.points$.index)) {
        gp <- gp + 
          build_geom(
            xy = pdr_drawing$.points |> dplyr::filter(.index == i), 
            tool = pdr_drawing$.strokes |> dplyr::filter(.index == i) |> dplyr::pull(.tool),
            tool_width = pdr_drawing$.strokes |> dplyr::filter(.index == i) |> dplyr::pull(.tool_width)
          )
      }
      pdr_drawing$.ggplot <- gp
    
      # And then capture matrix data from plot raster
      # And now capture the raster matrix data from plot
      capture_gfx <- agg_capture(pdr_drawing$.width, pdr_drawing$.height, "px")
      print(gp)
      scatter_mtx <- capture_gfx()
      dev.off()
      scatter_mtx <- scatter_mtx =="white"
      pdr_drawing$.rgba <- scatter_mtx

    } # end if
    
    pdr_drawing # map return
  }) # end map .. return this (modified pdr_list)
  return(pdr_list)
} # end func
