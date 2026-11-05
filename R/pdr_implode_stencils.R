#' Implode the stencils of multiple pain drawings into one
#'
#' Collapses the `.stencil` elements of several pain drawings into a single
#' stencil, by taking the union of the 1-regions of each `.stencil`. This is
#' useful, for instance, for combining several anatomical region stencils
#' into one larger region stencil.
#'
#' The function is intended for use inside [dplyr::summarise()], where the
#' result should be a single drawing per group. All input drawings must have
#' `.stencil` elements (see [pdr_add_stencil()]) and identical canvas
#' dimensions and `.units`.
#'
#' The result holds no strokes or points -- it is a stencil, not a drawing.
#' `.file`, `.version` and `.app` keep the common value if unique, otherwise
#' `"multiple"` (or `NA` for `.version`). `.timestamp` is the maximum of the
#' inputs (`NA` if all inputs are `NA`). The collapsed stencil is also stored
#' in the alpha channel of `.rgba` (as a black image), so that
#' [pdr_plot_stencil()] works on the result.
#'
#' @param pdr_list A pdr_list (see [pdr_match()]) where every pdr_drawing
#'   has a `.stencil` element (see [pdr_add_stencil()]). All drawings must
#'   have identical `.width`, `.height` and `.units`.
#' @param id The `.id` of the resulting pdr_drawing. If `NULL` (default),
#'   `"collapsed_<time>"` is used.
#'
#' @returns A pdr_list of length 1 with a single pdr_drawing whose `.stencil`
#'   is the union of the input stencils and whose `.stencil_inverted` is its
#'   complement. `.points` and `.strokes` are empty.
#' @seealso [pdr_add_stencil()], [pdr_implode()], [pdr_plot_stencil()]
#'
#' @export
#' @examples
#' \donttest{
#' pdr_stencil_full_body |>
#'   dplyr::summarise(pdr_data = pdr_implode_stencils(pdr_data))
#'
#' pdr_stencil_full_body |>
#'   dplyr::mutate(
#'     view = ifelse(stringr::str_detect(id, "^Back_|^Mid_back"), "Back", "Front")
#'   ) |>
#'   dplyr::summarise(
#'     pdr_data = pdr_implode_stencils(pdr_data),
#'     .by = view
#'   )
#' }
pdr_implode_stencils <- function(pdr_list, id = NULL) {
  pdr_list <- pdr_match(
    pdr_list,
    action = "coerce",
    as = "pdr_list",
    mode = "superficial"
  )

  # Sanity checks
  has_stencil <- purrr::map_lgl(pdr_list, \(x) ".stencil" %in% names(x))
  if (!all(has_stencil)) {
    cli::cli_abort(
      "All drawings in {.arg pdr_list} must have a {.field .stencil} element.
      See {.fn pdr_add_stencil}."
    )
  }

  widths <- purrr::map_int(pdr_list, ".width")
  heights <- purrr::map_int(pdr_list, ".height")
  if (length(unique(widths)) > 1 || length(unique(heights)) > 1) {
    cli::cli_abort(
      "All drawings must have identical {.field .width} and {.field .height}."
    )
  }

  stencil_dims_ok <- purrr::map_lgl(pdr_list, \(x) {
    identical(dim(x$.stencil), c(heights[[1]], widths[[1]]))
  })
  if (!all(stencil_dims_ok)) {
    cli::cli_abort(
      "All {.field .stencil} matrices must have dimensions height x width
      ({heights[[1]]} x {widths[[1]]})."
    )
  }

  units <- unique(purrr::map_chr(pdr_list, ".units"))
  if (length(units) > 1) {
    cli::cli_abort("All drawings must have the same {.field .units}.")
  }

  # Union of stencils: sum and cap at 1 (overlapping stencils allowed)
  stencil <- (Reduce(`+`, purrr::map(pdr_list, ".stencil")) > 0) + 0
  stencil_inverted <- 1 - stencil

  # Keep a metadata value if it is common to all inputs
  common_or <- function(var, otherwise) {
    values <- unique(purrr::map(pdr_list, var))
    if (length(values) == 1) values[[1]] else otherwise
  }

  timestamps <- stats::na.omit(purrr::map_chr(pdr_list, ".timestamp"))
  if (is.null(id)) {
    id <- paste0("collapsed_", Sys.time())
  }

  rgba <- array(0, dim = c(heights[[1]], widths[[1]], 4))
  rgba[,, 4] <- stencil

  list(
    list(
      .id = as.character(id),
      .file = common_or(".file", "multiple"),
      .version = common_or(".version", NA_integer_),
      .width = widths[[1]],
      .height = heights[[1]],
      .units = units,
      .timestamp = if (length(timestamps) == 0) {
        NA_character_
      } else {
        max(timestamps)
      },
      .app = common_or(".app", "multiple"),
      .points = tibble::tibble(
        .index = integer(),
        .x = integer(),
        .y = integer()
      ),
      .strokes = tibble::tibble(.index = integer()),
      .rgba = rgba,
      .stencil = stencil,
      .stencil_inverted = stencil_inverted
    )
  )
}
