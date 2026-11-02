#' Example pain drawings with clinical variables
#'
#' A simulated data set of 50 pain drawings with accompanying patient
#' variables. See [pdr_match()] for the definition of a pdr_tibble.
#'
#' @format A pdr_tibble (tibble) with 50 rows and 6 columns:
#' \describe{
#'   \item{id}{Character. Unique identifier.}
#'   \item{pdr_data}{pdr_list column with one pdr_drawing per row.}
#'   \item{sex}{Factor. `"Female"` or `"Male"`.}
#'   \item{age}{Numeric. Age in years.}
#'   \item{duration}{Factor. Pain duration category (e.g. `"Subacute"`,
#'     `"Chronic"`).}
#'   \item{pain}{Numeric. Pain intensity rating.}
#' }
#' @source Simulated; created in `data-raw/DATASET.R`.
"pdr_example_data"

#' Example pain drawings with simple geometric cases
#'
#' Ten small drawings (e.g. no area, duplicated points) useful for testing
#' geometry handling. See [pdr_match()] for the definition of a pdr_tibble.
#'
#' @format A pdr_tibble (tibble) with 10 rows and 2 columns:
#' \describe{
#'   \item{id}{Character. Name of the geometric case.}
#'   \item{pdr_data}{pdr_list column with one pdr_drawing per row.}
#' }
#' @source Created from MIRD-format strings in `data-raw/DATASET.R`.
"pdr_example_geometry"

#' Anatomical regions of the full body template, with stencils
#'
#' One drawing per anatomical region of the full body template, each with
#' `.rgba` and a `.stencil` / `.stencil_inverted` (see [pdr_add_stencil()]).
#' Intended for use as `stencils` in [pdr_summarize_rgba()] or as `stencil` in
#' [pdr_modify_rgba()]. See [pdr_match()] for the definition of a pdr_tibble.
#'
#' @format A pdr_tibble (tibble) with 46 rows and 2 columns:
#' \describe{
#'   \item{pdr_data}{pdr_list column with one pdr_drawing per region,
#'     including `.rgba`, `.stencil` and `.stencil_inverted`.}
#'   \item{id}{Character. Region name.}
#' }
#' @source Built from the files in `data-raw/regions_of_full_body_template/`
#'   by `data-raw/DATASET.R`.
"pdr_stencil_full_body"
