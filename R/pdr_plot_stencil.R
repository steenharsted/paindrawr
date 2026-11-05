#' Plot stencil data
#'
#' Plots the `.stencil` matrix of each drawing (or `.stencil_inverted` for the
#' inverted stencil) as a red mask. The function is primarily for checking
#' stencils created with [pdr_add_stencil()].
#'
#' It is a wrapper for [pdr_plot_rgba()]: for each drawing a red raster is
#' built whose alpha channel is the stencil, and the result is passed on to
#' [pdr_plot_rgba()]. The drawings do not need an `.rgba` element, and
#' `.data` does not need an `id` column; panels are identified by the `.id`
#' of each drawing.
#'
#' @inheritParams pdr_plot_rgba
#' @param .data A data frame (typically a pdr_tibble, see [pdr_match()]) with
#'   a pdr_list column whose pdr_drawings contain a `.stencil` element (and
#'   `.stencil_inverted` if `invert = TRUE`). The stencil elements are added
#'   by [pdr_add_stencil()].
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
#'     include_id = TRUE,
#'     invert = TRUE
#'   )
#' }
pdr_plot_stencil <- function(
  .data,
  pdr_col = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  ncol = NULL,
  nrow = NULL,
  invert = FALSE
) {
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data frame with a pdr_list column, not
      {.obj_type_friendly {(.data)}}."
    )
  }

  # Validate here so errors point at pdr_plot_stencil()
  background_image <- read_background_image(background_image)

  drawings <- dplyr::pull(.data, {{ pdr_col }})
  stencil_name <- if (invert) ".stencil_inverted" else ".stencil"

  has_stencil <- purrr::map_lgl(drawings, \(pdr_drawing) {
    is.list(pdr_drawing) && is.matrix(pdr_drawing[[stencil_name]])
  })
  if (!all(has_stencil)) {
    missing <- which(!has_stencil)
    cli::cli_abort(
      c(
        "All drawings must have a {.field {stencil_name}} matrix.",
        "x" = "Missing in {cli::qty(length(missing))}drawing{?s} {missing}.",
        "i" = "Add stencils with {.fn pdr_add_stencil}."
      )
    )
  }

  # A red raster whose alpha channel is the stencil
  stencil_drawings <- purrr::imap(drawings, \(pdr_drawing, i) {
    stencil <- pdr_drawing[[stencil_name]]
    rgba <- array(0, dim = c(dim(stencil), 4))
    rgba[,, 1] <- 1
    rgba[,, 4] <- stencil
    id <- if (is.null(pdr_drawing$.id)) as.character(i) else pdr_drawing$.id
    list(.id = id, .rgba = rgba)
  })

  tibble::tibble(pdr_data = stencil_drawings) |>
    pdr_plot_rgba(
      pdr_col = pdr_data,
      background_image = background_image,
      include_id = include_id,
      ncol = ncol,
      nrow = nrow
    )
}
