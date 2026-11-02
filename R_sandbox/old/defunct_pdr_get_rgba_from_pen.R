pdr_get_rgba_from_pen <- function(pdr_list) {
  #### Sanity checks
    if(!is.list(pdr_list) || 
     !all(pdr_list |> purrr::map_lgl(~is.list(.x))) ||
     !all(pdr_list |> purrr::map_lgl(~".points" %in% names(.x)))) {
        warning("Parameter 'pdr_list' is not a list of lists with a named element '.points'.")
        stop()
    }

  pdr_list |> purrr::map(\(pdr_drawing) {
    if(identical(NA, pdr_drawing$.points)) {
      pdr_drawings.rgba <- NA
    } else {
      image_width <- pdr_drawing$.width
      image_height <- pdr_drawing$.height
      base_plot <- 
        dplyr::left_join(pdr_drawing$.points, pdr_drawing$.strokes, by=".index") |>
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
          ggplot2::scale_linewidth_identity() +
          ggplot2::geom_path(
            ggplot2::aes(
              group = paste0(.id, "_", .index),
              linewidth = size_mm + 1
            ),
            linetype = 1
          )
    }
  })
}