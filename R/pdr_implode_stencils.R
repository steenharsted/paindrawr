# DRAFT: pdr_implode_stencils() -------------------------------------------
# Intended for use inside summarise(): collapses the stencils of a pdr_list
# into a single stencil (the union of the 1-regions of each `.stencil`).
# Assumes 1 = inside the region (pdr_add_stencil(threshold = 0, by = ">")).
#
# Returns a pdr_list of length 1 holding one pdr_drawing with:
#   .id, .width, .height, .units  - as input (id from `id` argument)
#   .file, .version, .app         - common value if unique, else "multiple"/NA
#   .timestamp                    - max of inputs (NA if all NA)
#   .points, .strokes             - empty (the result is a stencil, not strokes)
#   .rgba                         - black, alpha = collapsed stencil, so that
#                                   pdr_plot_stencil() works
#   .stencil, .stencil_inverted   - collapsed stencil and its complement
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
