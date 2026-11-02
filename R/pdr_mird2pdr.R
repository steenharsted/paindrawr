#' Convert MIRD pain drawings to a pdr_list
#'
#' MIRD exports pain drawings in a custom format like:
#' `(,1,2,5,4,),(,8,8,7,7,)`
#' which corresponds to:
#'
#' | x | y | index |
#' |---|---|-------|
#' | 1 | 2 | 1 |
#' | 5 | 4 | 1 |
#' | 8 | 8 | 2 |
#' | 7 | 7 | 2 |
#'
#' This function takes a vector of length n of such MIRD strings and converts
#' it into a pdr_list of length n (see [pdr_match()] for the terminology).
#' Strings that are `NA` or too short to hold a point give a pdr_drawing
#' without points and strokes.
#'
#' The intended use case is a data frame (tibble) of MIRD data including pain
#' drawing data. A new pdr_list column can be generated with a simple
#' `dplyr::mutate()` call.
#'
#' Each of the parameters supplied (except `mird_pd`) must be either of the
#' same length as `mird_pd` or of length 1, in which case it is repeated.
#'
#' @param mird_pd A character vector of MIRD pain drawings.
#' @param id A unique id for each drawing (default: `seq_along(mird_pd)`).
#' @param file File name (default `""`).
#' @param version Version (default `""`).
#' @param width Canvas width, integer (default `450L`).
#' @param height Canvas height, integer (default `500L`).
#' @param units Units (default `"px"`).
#' @param timestamp Timestamp (default `""`).
#' @param app App name (default `""`).
#' @param tool Drawing tool (default `"pen"`).
#' @param draw_input_type Input type (default `"mouse"`).
#' @param tool_width Tool width, integer (default `1L`).
#' @param point_density Point density, integer (default `1L`).
#' @param spray_radius Spray radius, integer (default `1L`).
#' @param alpha Alpha, numeric between 0 and 1 (default `1.0`).
#' @param color Stroke colour (default `"#ff0000"`).
#' @param flipy Logical (default `TRUE`). If `TRUE`, the y-axis is flipped
#'   (via [pdr_modify_points()] with `op = "flipy"`), because MIRD has its
#'   origin in the upper left corner whereas paindrawr uses the lower left.
#'
#' @returns A pdr_list (see [pdr_match()]) of length `length(mird_pd)`.
#'
#' @export
#' @examples
#' mird <- c(
#'   "(,460,709,463,709,464,719,448,720,449,712,)",
#'   "(,393,682,430,672,429,682,)",
#'   NA
#' )
#' pdr_list <- pdr_mird2pdr(mird, id = c("a", "b", "c"))
#' pdr_list[[1]]$.points
#'
#' # Typical use: create a pdr_list column in a tibble
#' tibble::tibble(id = c("a", "b", "c"), mird = mird) |>
#'   dplyr::mutate(pdr_data = pdr_mird2pdr(mird, id = id))
pdr_mird2pdr <- function(
  mird_pd,
  id = seq_along(mird_pd),
  file = "",
  version = "",
  width = as.integer(450),
  height = as.integer(500),
  units = "px",
  timestamp = "",
  app = "",
  tool = "pen",
  draw_input_type = "mouse",
  tool_width = as.integer(1),
  point_density = as.integer(1),
  spray_radius = as.integer(1),
  alpha = as.numeric(1.0),
  color = "#ff0000",
  flipy = TRUE
) {
  #### Sanity check
  if (!is.character(mird_pd)) {
    warning(
      "In'pdr_mird2pdr()' parameter 'mird_pd' is wrong type -- expected character vector. "
    )
  }

  if (!is.numeric(alpha) || anyNA(alpha) || any(alpha < 0 | alpha > 1)) {
    cli::cli_abort("{.arg alpha} must be numeric values between 0 and 1.")
  }

  # All parameters must be of length 1 or the length of `mird_pd`
  n <- length(mird_pd)
  params <- list(
    id = id,
    file = file,
    version = version,
    width = width,
    height = height,
    units = units,
    timestamp = timestamp,
    app = app,
    tool = tool,
    draw_input_type = draw_input_type,
    tool_width = tool_width,
    point_density = point_density,
    spray_radius = spray_radius,
    alpha = alpha,
    color = color
  )
  param_lengths <- lengths(params)
  bad <- names(params)[!param_lengths %in% c(1L, n)]
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Parameters must have length 1 or the length of {.arg mird_pd} ({n}).",
      rlang::set_names(
        paste0("{.arg ", bad, "} has length ", param_lengths[bad], "."),
        rep("x", length(bad))
      )
    ))
  }

  #### Helper functions
  # This function is vectorized and returns a list
  # Each string in mird_pd becomes an element in the list
  # Each string corresponds to a pain drawing which may
  # contain one or more strokes -- if no strokes: NA

  convert_mird_to_tibble <- function(ms) {
    # This function converts a single mird string into a
    # tibble of x and y coordinate points.
    # The ms mird string looks like:
    # e.g. "(,293,201,300,202,303,202,)"
    if (is.na(ms) || !is.character(ms) || stringr::str_length(ms) < 7) {
      # "(,x,y,)" is length 7
      #return(NA)
      return(tibble::tibble(
        .index = as.integer(),
        .x = as.integer(),
        .y = as.integer()
      ))
    } else {
      original_ms <- ms
      # Simplify notation
      # Go from (,1,2,5,4,),(,8,8,7,7,)
      # to          1,2,5,4;8,8,7,7
      ms <- stringr::str_replace_all(ms, ",\\),\\(,", ";") # ,),(,  --> ;
      ms <- stringr::str_replace_all(ms, "\\(,", "") # (,     -->
      ms <- stringr::str_replace_all(ms, ",\\)", "") # ),     -->

      points <- stringr::str_split(ms, ";", simplify = TRUE) # split to chr [1, 1:2] "1,2,5,4" "8,8,7,7"
      points <- stringr::str_split(points, ",") # split to list [[1]] [1] "1" "2" "5" "4" [[2]] [1] "8" "8" "7" "7"
      # Check for even number of points?
      points <- points |>
        purrr::imap_dfr(\(xy, i) {
          # Each stroke must hold x and y values in pairs. An odd number means
          # the data is corrupt (a coordinate is missing), so we fail rather
          # than guess: matrix() would silently recycle values to fill the gap.
          if (length(xy) %% 2 != 0) {
            cli::cli_abort(c(
              "Invalid MIRD string: a stroke has an odd number of coordinates.",
              "x" = "Stroke {i} has {length(xy)} values; x and y must be paired.",
              "i" = "String: {.val {stringr::str_trunc(original_ms, 80)}}"
            ))
          }
          xy <- as.integer(xy) |>
            matrix(ncol = 2, byrow = TRUE) |>
            tibble::as_tibble(.name_repair = ~ c(".x", ".y"))
          xy$.index <- as.integer(i)
          xy # End imap iteration here
        })
      return(points)
    }
  }

  convert_points_to_strokes <- function(
    p,
    tool,
    draw_input_type,
    tool_width,
    point_density,
    spray_radius,
    alpha,
    color
  ) {
    # This function creates a new .strokes tibble from a
    # .points tibble
    if (nrow(p) == 0) {
      return(
        tibble::tibble(
          .index = as.integer(),
          .tool = as.character(),
          .draw_input_type = as.character(),
          .tool_width = as.integer(),
          .point_density = as.integer(),
          .spray_radius = as.integer(),
          .alpha = as.numeric(),
          .color = as.character()
        )
      )
    } else {
      tibble::tibble(
        .index = unique(p$.index),
        .tool = tool,
        .draw_input_type = draw_input_type,
        .tool_width = as.integer(tool_width),
        .point_density = as.integer(point_density),
        .spray_radius = as.integer(spray_radius),
        .alpha = as.numeric(alpha),
        .color = color
      )
    }
  }

  #### This does the actual conversion from MIRD string to
  # valid pain drawing data list column. Each element is a list of named
  # elements (.id, .file, ..etc) -- most are parameters passed when calling
  # this function (e.g. .width and .height), while .points and .strokes are
  # calculated from the MIRD pain drawing strings.
  pdr_list <-
    purrr::pmap(
      # All variables to include (each must be length n or 1)
      list(
        mird_pd,
        id,
        file,
        version,
        width,
        height,
        units,
        timestamp,
        app,
        tool,
        draw_input_type,
        tool_width,
        point_density,
        spray_radius,
        alpha,
        color
      ),
      # Name for iteration instance of each variable
      \(
        i_mird_pd,
        i_id,
        i_file,
        i_version,
        i_width,
        i_height,
        i_units,
        i_timestamp,
        i_app,
        i_tool,
        i_draw_input_type,
        i_tool_width,
        i_point_density,
        i_spray_radius,
        i_alpha,
        i_color
      ) {
        # Function
        i_points <- convert_mird_to_tibble(i_mird_pd) # tibble
        i_strokes <- convert_points_to_strokes(
          p = i_points,
          tool = i_tool,
          draw_input_type = i_draw_input_type,
          tool_width = i_tool_width,
          point_density = i_point_density,
          spray_radius = i_spray_radius,
          alpha = i_alpha,
          color = i_color
        )
        list(
          .id = i_id,
          .file = i_file,
          .version = i_version,
          .width = i_width,
          .height = i_height,
          .units = i_units,
          .timestamp = i_timestamp,
          .app = i_app,
          .points = i_points,
          .strokes = i_strokes
        )
      }
    )

  if (flipy) {
    pdr_list <- pdr_list |> pdr_modify_points(op = "flipy")
  }

  # Return this
  pdr_list
}
