#' Convert MiRD pain drawing strings to coordinate data
#'
#' Parses pain drawing data stored in the legacy *MiRD* string format and
#' converts it into a list-column of stroke coordinate tibbles compatible with
#' the `pdr_data` column of a pain drawing data structure (see [pdr_check_data()]).
#'
#' Each input string represents one pain drawing and is converted into a tibble
#' with columns `i`, `x`, and `y`, where `i` indexes individual strokes.
#'
#' @details
#' **MiRD string format**
#'
#' A MiRD pain drawing is encoded as a character string containing one or more
#' strokes. Each stroke is represented as a sequence of x/y coordinate pairs:
#'
#' * Coordinates are stored as a flat sequence: `(,x1,y1,x2,y2,...)`
#' * Multiple strokes are concatenated as:
#'   `(,x1,y1,...,),(,x1,y1,...)`
#'
#' This function:
#'
#' * Splits the string into individual strokes
#' * Converts coordinate sequences into `(x, y)` pairs
#' * Assigns a stroke index `i` to each group of coordinates
#'
#' **Coordinate system**
#'
#' MiRD uses a coordinate system with origin in the upper-left corner.
#' so it may be necessary to 'flip' the y axis -- see pdr_modify()
#'
#' **Missing or invalid input**
#'
#' * `NA` or malformed strings return `NA` in place of .points and .strokes
#' * No validation of coordinate consistency (e.g. odd-length sequences) is performed
#'
#' @param mird_string Character vector of MiRD-encoded pain drawing strings.
#'
#' @param ids A list or vector of unique strings which must be of same length as mird_string.
#' @param file A list or vector (strings) which is empty, of length 1 or of same length as mird_string
#' @param version A list or vector (strings) which is empty, of length 1 or of same length as mird_string
#' @param width An list or vector (integers) which is empty, of length 1 or of same length as mird_string
#' @param height An list or vector (integers) which is empty, of length 1 or of same length as mird_string
#' @param units A list or vector (strings) which is empty, of length 1 or of same length as mird_string
#' @param timestamp A list or vector (strings) which is empty, of length 1 or of same length as mird_string
#' @param app A list or vector (strings) which is empty, of length 1 or of same length as mird_string
#' @param tool A list or vector (strings) which is empty, of length 1 or of same length as mird_string. If
#' tool is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#' @param draw_input_type A list or vector (strings) which is empty, of length 1 or of same length as mird_string. If
#' dar_input_type is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#' @param tool_width A list or vector (integers) which is empty, of length 1 or of same length as mird_string. If
#' tool_width is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#' @param point_density A list or vector (integers) which is empty, of length 1 or of same length as mird_string. If
#' point_density is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#' @param spray_radius A list or vector (integers) which is empty, of length 1 or of same length as mird_string. If
#' spray_radius is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#' @param alpha A list or vector (integers) which is empty, of length 1 or of same length as mird_string. If
#' alpha is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#' @param color A list or vector (strings) which is empty, of length 1 or of same length as mird_string. If
#' color is a list of the same length as mird_string, each element should be a list or vector
#' which is of length 1 or same length as the number of strokes in that mird string.
#'
#' @returns
#' A list of lists adhering to the format of a pain drawing
#' list-col. Any of the other data stored in a pain drawing
#' lost-col can be included as parameters -- sush as 'file',
#' 'version', etc. This also applies to parameters stored in
#' the '.strokes. element, such as 'tool', 'alpha', etc.
#'
#' @export
#'
#' @examples
#' mird_tibble <- tibble::tibble(
#'   ids = letters[1:4],
#'   mird_string = c(
#'     NA,
#'     "(,293,201,300,202,303,202,)",
#'     "(,198,104,206,103,),(,203,100,209,108,)",
#'     "(,335,215,336,215,),(,331,211,331,212,331,213,331,214,),(,341,263,),(,341,272,),(,341,287,)"
#'     )
#' )
#'
#' # Convert to list-column of coordinate tibbles
#' mird_tibble |> mutate(pdr_data =  pdr_import_mird(mird_string, ids, app = "my_app", file = c("file1", "file2", "file3", "file4")))
#'
#'
pdr_import_mird <- function(
  mird_string = "mird_string",
  ids = "id",
  file = "",
  version = "",
  width = as.integer(450),
  height = as.integer(500),
  units = "px",
  timestamp = as.character(Sys.time()),
  app = "",
  tool = "pen",
  draw_input_type = "mouse",
  tool_width = as.integer(1),
  point_density = as.integer(1),
  spray_radius = as.integer(1),
  alpha = as.integer(255),
  color = "#ff0000"
) {
  # This function takes as input ...

  if (!is.character(mird_string) || length(mird_string) < 1) {
    stop("Parameter `mird_string` must be a vector of characters of length 1+")
  }
  ids <- unique(as.character(ids))
  if (length(mird_string) != length(ids)) {
    stop("MIRD and (unique) IDs must be defined and of same length.")
  }

  # This function is vectorized and returns a list
  # Each string in mird_string becomes an element in the list
  # Each string corresponds to a pain drawing which may
  # contain one or more strokes -- if no strokes: NA

  convert_mird_to_tibble <- function(ms) {
    if (is.na(ms) || !is.character(ms) || stringr::str_length(ms) < 7) {
      return(NA)
      #return(tibble::tibble(.index = as.integer(), .x=as.integer(), .y=as.integer()))
    } else {
      # Simplify notation
      # Go from (,1,2,5,4,),(,8,8,7,7,)
      # to          1,2,5,4;8,8,7,7
      ms <- stringr::str_replace_all(ms, ",\\),\\(,", ";") # ,),(,  --> ;
      ms <- stringr::str_replace_all(ms, "\\(,", "") # (,     -->
      ms <- stringr::str_replace_all(ms, ",\\)", "") # ),     -->

      points <- stringr::str_split(ms, ";", simplify = TRUE)
      points <- stringr::str_split(points, ",")
      # Check for even number of points?
      points <- points |>
        purrr::imap_dfr(\(xy, i) {
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
    if (identical(NA, p)) {
      NA
    } else {
      tibble::tibble(
        .index = unique(p$.index),
        .tool = tool,
        .draw_input_type = draw_input_type,
        .tool_width = as.integer(tool_width),
        .point_density = as.integer(point_density),
        .spray_radius = as.integer(spray_radius),
        .alpha = as.integer(alpha),
        .color = color
      )
    }
  }

  # Convert to a valid pain drawing data structure - we do
  # by mapping MULTIPLE variables which should all have the
  # same length or length==1 (to be reused)
  list_col <-
    list(
      mird_string,
      ids,
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
    ) |>
    purrr::pmap(\(
      mird_string,
      ids,
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
      color,
      indx
    ) {
      tmp_points <- convert_mird_to_tibble(mird_string)
      list(
        .id = ids,
        .file = file,
        .version = version,
        .width = as.integer(width),
        .height = as.integer(height),
        .units = units,
        .timestamp = as.character(timestamp),
        .app = app,
        .points = tmp_points,
        .strokes = convert_points_to_strokes(
          tmp_points,
          tool,
          draw_input_type,
          tool_width,
          point_density,
          spray_radius,
          alpha,
          color
        )
      )
    })

  #warning("You may want to emply the 'flipy' functionality of the pdr_modify function to MIRD data")
  tibble::tibble(pdr_data = list_col) |> mutate(id = pdr_get_info(pdr_data, ".id")) # return
}
