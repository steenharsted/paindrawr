#' Implode multiple pain drawwings into one.
#'
#' This function takes as input a pain drawing data structure, 
#' with multiple elements, i.e. pain drawings. The strokes
#' of these are imploded (or collated) into a single pain drawing 
#' with all strokes (including NA) retained in the original 
#' order.
#' 
#' The function is useful, for instance, for imploding anatomcial 
#' regions into larger anatomical regions which can then be
#' merged (via `pdr_modify()`) into a sinigle larger region.
#' 
#' #' **NOTE** The output of this function is (likely) not of same length as input.
#' 
#' @param pdr must be a valid pain drawing data structure. 
#' Furthermore, each element in `pdr` should have the same 
#' `.units`. If elements of `pdr` has different canvas widths
#' or heights, they will all be reset to the maximum canvas
#' dimensions.
#' @returns A new pain drawing data structure with a single
#' element with all strokes from the input pain drawings
#' retained. Note, that stroke indeces are reset and in
#' `.strokes` and `.points`. The `.id` element is also reset.
#'
#' @export
#' @examples
#' pdr_implode(pd_demo_anatomy)
#' 
#' bi <- pdr_example("mird_body_background")
#' pd_demo_anatomy |> pd_recreate_drawing(background_image = bi) # Note: execution time > 5 sec
#' pd_demo_anatomy |> pd_implode() |> pd_recreate_drawing(background_image = bi)
#' pd_demo_anatomy |> pd_implode() |> pd_sanitize(overlaps="merge") |> pd_recreate_drawing(background_image = bi)
#' 
#' pd_demo_anatomy |> dplyr::filter(id %in% c("Front_left_thigh", "Front_left_leg", "Front_left_foot")) |> pd_recreate_drawing(background_image = bi)#' 
#' pd_demo_anatomy |> dplyr::filter(id %in% c("Front_left_thigh", "Front_left_leg", "Front_left_foot")) |> pd_implode() |> pd_recreate_drawing(background_image = bi)
#' 
pdr_implode <- function(.data, paindrawr_data = pdr_data) {
  pdr <- .data |> dplyr::pull({{paindrawr_data}})

  # Sanity checks
  # If only one pain drawing -- return input
  if(length(pdr)==1) { return(pdr)}
  # pdr should include only one type of units 
  if (length(unique(pdr |> purrr::map_chr(\(x) {x$.units})))>1) {
    warning("Can not implode pain drawings with more than one type of unit.")
    return(NA)
  }
  # pdr width and height should be similar -- else, choose largest
  if(length(unique(pdr |> purrr::map_int(\(x) {x$.width})))>1 ||
     length(unique(pdr |> purrr::map_int(\(x) {x$.height})))>1) {
    warning("Different canvas widths and/or heights -- all coerced to max width and height, respectively.")
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
    var_values <- pdr |> purrr::map_depth(.depth=1, {{var}}) |> purrr::as_vector()
    ifelse(
      length(unique(var_values))>1, 
      "multiple", 
      unique(var_values))
  }

  tmp_id <- paste0("imploded_", Sys.time())
  result <- list(
    list(
      .id = tmp_id,
      .file = one_or_multiple(pdr, ".file"),
      .version = one_or_multiple(pdr, ".version"),
      .width = max(pdr |> purrr::map_int(\(x) {x$.width})),  
      .height = max(pdr |> purrr::map_int(\(x) {x$.height})),
      .units = pdr[[1]]$.units, 
      .timestamp = max(pdr |> purrr::map_chr(\(x) {x$.timestamp})),
      .app = one_or_multiple(pdr, ".app"),
      .strokes = {pdr |> 
        purrr::imap_dfr(\(x, indx) {rbind(x$.strokes) |> dplyr::mutate(pdr_indx = indx)}) |>
          dplyr::group_by(pdr_indx, .index) |>
          dplyr::mutate(.index =dplyr::cur_group_id()) |>
          dplyr::ungroup() |>
          dplyr::select(-pdr_indx)},
      .points = {pdr |> 
        purrr::imap_dfr(\(x, indx) {rbind(x$.points) |> dplyr::mutate(pdr_indx = indx)}) |>
          dplyr::group_by(pdr_indx, .index) |>
          dplyr::mutate(.index =dplyr::cur_group_id()) |>
          dplyr::ungroup() |>
          dplyr::select(-pdr_indx)}
    )
  )

  result <- tibble::tibble(
    id = tmp_id,
    pdr = result
  ) |> dplyr::rename({{paindrawr_data}} := pdr)

  return(result)
}
