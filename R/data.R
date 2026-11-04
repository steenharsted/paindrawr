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

#' Example hand drawings of imagined pain (palmar view)
#'
#' A small, deliberately silly data set collected to demonstrate the PainDraw
#' and paindrawr workflows. Participants first chose between a hammer and an
#' apple, and then a distance (10 cm or 1 m). They were then told that the
#' chosen object had fallen from that height and landed on their left hand,
#' and were asked to draw their imaginary hand pain on a palmar lower arm
#' template.
#'
#' One participant (`id` 22) has a missing drawing (and missing `tool` and
#' `distance`). The drawing is `NULL`, which is useful for demonstrating how
#' missing drawings are handled as opposed to empty drawings. File paths and
#' timestamps have been removed from the drawings. Use `pdr_add_rgba()` to
#' rasterise the drawings. The matching background image is
#' `bg_lower_arm_palmar.png` in `system.file("extdata", package = "paindrawr")`.
#'
#' @format A tibble with 30 rows and 4 columns:
#' \describe{
#'   \item{id}{Integer. Participant (record) identifier.}
#'   \item{palmar}{List column with one drawing per row (`NULL` if missing).}
#'   \item{tool}{Factor. Object that fell on the hand: `"Apple"` or
#'     `"Hammer"`.}
#'   \item{distance}{Factor. Height the object fell from: `"10 centimeters"`
#'     or `"1 meter"`.}
#' }
#' @source Collected by the package authors; created in
#'   `data-raw/pdr_example_palmar.R`.
"pdr_example_palmar"

#' Example drawings of locations on Zealand
#'
#' A small, deliberately silly data set collected to demonstrate the PainDraw
#' and paindrawr workflows. Participants first rated their knowledge of the
#' geography of East Denmark from 0 ("I think Copenhagen is in Sweden") to 10
#' ("I can name every cobblestone in Nyhavn"). They were then asked to draw one
#' or two circles around the cities Holbæk and Præstø, as small as
#' possible while still including both cities.
#'
#' One participant (`id` 22) has a missing drawing, stored as `NULL`. File
#' paths and timestamps have been removed from the drawings. Use
#' `pdr_add_rgba()` (optionally with `polygon = "both"`) to rasterise the
#' drawings. The matching background image is `bg_zealand.png` in
#' `system.file("extdata", package = "paindrawr")`.
#'
#' @format A tibble with 30 rows and 3 columns:
#' \describe{
#'   \item{id}{Integer. Participant (record) identifier.}
#'   \item{zealand}{List column with one drawing per row (`NULL` if missing).}
#'   \item{geography}{Integer. Self-rated knowledge of the geography of east
#'     Denmark, from 0 to 10; `NA` if not answered.}
#' }
#' @source Collected by the package authors; created in
#'   `data-raw/pdr_example_zealand.R`.
"pdr_example_zealand"
