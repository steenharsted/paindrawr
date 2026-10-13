pdr_make_rgba_template <- function(pdr_list, inverse=FALSE, antialiase="loose") {
  #### Sanity checks
  # ...ONLY ONE STROKE

  if(!antialiase %in% c("strict", "loose")) {
    antialiase <- "loose"
    warning("Unexpected value of parameter 'antialiase' -- defaulting to 'loose'.")
  }

  pdr_list |> purrr::map(\(pdr_drawing) {
    capture_gfx <- agg_capture(pdr_drawing$.width, pdr_drawing$.height, "px")
    pdr_drawing$.points |> 
      ggplot2::ggplot(aes(x=.x, y=.y)) +
        ggplot2::scale_x_continuous(limits = c(0, pdr_drawing$.width), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, pdr_drawing$.height), expand = c(0, 0)) +
        ggplot2::geom_polygon(fill="black") + 
        ggplot2::theme_void()     
    scatter_mtx <- capture_gfx()
    invisible(dev.off())
    # Reduce all value from gray scale to TRUE/FALS. 
    if(antialiase=="strict" && !inverse) { scatter_mtx <- scatter_mtx == "black" }
    if(antialiase=="strict" && inverse) { scatter_mtx <- scatter_mtx != "black" }
    if(antialiase=="loose" && !inverse) { scatter_mtx <- scatter_mtx != "white" }
    if(antialiase=="loose" && inverse) { scatter_mtx <- scatter_mtx == "white" }
    pdr_drawing$.rgba <- scatter_mtx
    pdr_drawing # Return this as result
  })
 

}