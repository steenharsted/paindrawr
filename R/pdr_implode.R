#' Implode multiple pain drawings into one
#'
#' Collates the strokes of several pain drawings into a single pain drawing,
#' with all strokes (including `NA`) retained in the original order.
#'
#' The function is useful, for instance, for imploding anatomical regions
#' into a larger anatomical region, which can then be merged (e.g. via
#' [pdr_modify_rgba()]) into a single larger region.
#'
#' **NOTE** The output of this function is (likely) not of the same length as
#' the input.
#'
#' If `pdr_col` holds only one drawing, it is returned as is (as the pdr_list
#' pulled from `.data`, not as a pdr_tibble). If the drawings have different
#' `.units`, a warning is given and `NA` is returned. If they have different
#' canvas widths or heights, a warning is given and the maximum width and
#' height are used. For `.file`, `.version` and `.app` the common value is kept
#' if unique, otherwise `"multiple"`. `.timestamp` is the maximum of the
#' inputs. See [pdr_match()] for the terminology (pdr_tibble, pdr_list,
#' pdr_drawing).
#'
#' @param .data A pdr_tibble (see [pdr_match()]). The drawings in `pdr_col`
#'   should all have the same `.units`.
#' @param pdr_col The column in `.data` holding the pdr_list to implode
#'   (default `pdr_data`). The result has a column of the same name.
#'
#' @returns A pdr_tibble (see [pdr_match()]) with one row: `id` is
#'   `"imploded_<time>"` and `pdr_col` holds a pdr_list with a single
#'   pdr_drawing containing all strokes and points of the input drawings.
#'   Stroke indices (`.index`) in `.strokes` and `.points` are renumbered, and
#'   `.id` is reset to the same `"imploded_<time>"` value. Any `.rgba` or
#'   `.stencil` elements are not carried over.
#'
#' @export
#' @examples
#' pdr_example_data |> pdr_implode()
#'
#' # Compare plots of three original drawings and the imploded drawing
#' bg <- pdr_example_file("bg_zealand.png")
#' three <- pdr_example_data[1:3, ]
#' three |> pdr_plot_xy(background_image = bg)
#' three |> pdr_implode() |> pdr_plot_xy(background_image = bg)
pdr_implode <- function(.data, pdr_col = pdr_data) {
  pdr <- .data |> dplyr::pull({{ pdr_col }})

  # Sanity checks
  # If only one pain drawing -- return input
  if (length(pdr) == 1) {
    return(pdr)
  }
  # pdr should include only one type of units
  if (
    length(unique(
      pdr |>
        purrr::map_chr(\(x) {
          x$.units
        })
    )) >
      1
  ) {
    warning("Can not implode pain drawings with more than one type of unit.")
    return(NA)
  }
  # pdr width and height should be similar -- else, choose largest
  if (
    length(unique(
      pdr |>
        purrr::map_int(\(x) {
          x$.width
        })
    )) >
      1 ||
      length(unique(
        pdr |>
          purrr::map_int(\(x) {
            x$.height
          })
      )) >
        1
  ) {
    warning(
      "Different canvas widths and/or heights -- all coerced to max width and height, respectively."
    )
  }

  # Result needs to be a tibble with just a single pain
  # drawing -- i.e 'pdr_data' should be a list of just one
  # element (.id ... .points)
  # We build that by hand keeping a close eye on data types
  #   * `.id`: character (unique identifier)
  #   * `.version`, `.width`, `.height`: integer
  #   * `.file`, `.units`, `.timestamp`, `.app`: character or NA
  #   * `.strokes`, `.points`: tibble or NA
  #
  # Id must always be unique, so we simple set it to 'imploded'
  # and a timestamp
  #
  # For version, width, height, and timestamp we pick the
  # largest value in input (smallest for version!)
  #
  # For file and app we choose the input if there is only one
  # value in input, otherwise 'multiple'
  #
  # For units -- we pick the first input value, because sanity
  # checks above would fail if multiple input values
  #
  # For stroke and poitns, we concatenate multiple data frames
  # into one (each)

  one_or_multiple <- function(pdr, var) {
    # This helper function returns either the string "multiple"
    # or the one value found for 'var' in 'pdr' (if unique)
    # We use this to retain variable values where there is only one
    # or replace multiple my "multiple" ... when imploding data
    var_values <- pdr |>
      purrr::map_depth(.depth = 1, {{ var }}) |>
      purrr::as_vector()
    ifelse(
      length(unique(var_values)) > 1,
      "multiple",
      unique(var_values)
    )
  }

  tmp_id <- paste0("imploded_", Sys.time())
  result <- list(
    list(
      .id = tmp_id,
      .file = one_or_multiple(pdr, ".file"),
      .version = one_or_multiple(pdr, ".version"),
      .width = max(
        pdr |>
          purrr::map_int(\(x) {
            x$.width
          })
      ),
      .height = max(
        pdr |>
          purrr::map_int(\(x) {
            x$.height
          })
      ),
      .units = pdr[[1]]$.units,
      .timestamp = max(
        pdr |>
          purrr::map_chr(\(x) {
            x$.timestamp
          })
      ),
      .app = one_or_multiple(pdr, ".app"),
      .strokes = {
        pdr |>
          purrr::imap_dfr(\(x, indx) {
            rbind(x$.strokes) |> dplyr::mutate(pdr_indx = indx)
          }) |>
          dplyr::group_by(pdr_indx, .index) |>
          dplyr::mutate(.index = dplyr::cur_group_id()) |>
          dplyr::ungroup() |>
          dplyr::select(-pdr_indx)
      },
      .points = {
        pdr |>
          purrr::imap_dfr(\(x, indx) {
            rbind(x$.points) |> dplyr::mutate(pdr_indx = indx)
          }) |>
          dplyr::group_by(pdr_indx, .index) |>
          dplyr::mutate(.index = dplyr::cur_group_id()) |>
          dplyr::ungroup() |>
          dplyr::select(-pdr_indx)
      }
    )
  )

  result <- tibble::tibble(
    id = tmp_id,
    pdr = result
  ) |>
    dplyr::rename({{ pdr_col }} := pdr)

  return(result)
}
