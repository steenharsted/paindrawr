#' @keywords internal
#' @section Terminology:
#' | Term | Meaning |
#' |---|---|
#' | pdr_drawing | Named list for one drawing: `.id`, `.file`, `.version`, `.width`, `.height`, `.units`, `.timestamp`, `.app`, `.strokes`, `.points`, optional `.rgba`, `.stencil`, `.stencil_inverted` |
#' | pdr_list | A list of pdr_drawings (typically the list-column `pdr_data`) |
#' | pdr_tibble | A tibble with columns `id` and a pdr_list column (default name `pdr_data`) |
#'
#' See [pdr_match()] for validating and coercing between these types.
"_PACKAGE"

#' @importFrom stats quantile sd median IQR runif
NULL
