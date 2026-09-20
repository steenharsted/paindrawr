#' Convert raw pain drawing coordinates into '_simple feature_' (sf) geometry polygons.
#'
#' The function will ensure that all pain drawing strokes are represented as a POLYGON object -- point and lines will be buffered and cast as polygon, multipolygon obejcts, which may arise from selv-intersecting strokes, will be buffered, merged by union and cast as a polygons.
#'
#' @param pdr_list A valid pain drawing list-col -- see `pdr_check_data()` for details
#'
#' @param buffer A single integer value > 0 and < ½ the smaller of the two canvas dimension, specifying the distance of polygon buffering. If set to -1 (default), the buffer distance will be set according to the tool width  for each stroke as defined in 'pdr'. Note, that buffering may enlarge polygons beyond the canvas limits and will be trimmed accordingly.
#'
#' @returns A modified version of the input parameter `pdr` with a new `.polygons` element added to each pain drawing. The new .polygon element is a _simple feature_ geometric object (POLYGON) -- see sf package for details.
#'
#' @export
#' @examples
#'
#' pdr_example_geometry |> mutate(pdr_data = pdr_add_polygons(pdr_data))
#'
#' pdr_example_geometry |> mutate(pdr_data = pdr_add_polygons(pdr_data, buffer=0))
#'
#'
pdr_add_polygons <- function(pdr_list, buffer=-1) {
  # pdr_list is assumed to be valid pain drawing list-col data

  # If user specifies buffer, it must be a positive integer 1+ and smaller than half the smallest canvas dimensions.
  if (buffer != -1) {
    buffer = abs(as.integer(buffer[[1]]))
    min_width <- pdr_list |> purrr::map_int(\(df) df$.width) |> min()
    min_height <- pdr_list |> purrr::map_int(\(df) df$.height) |> min()
    if(!(buffer < 0.5*min_width & buffer < 0.5*min_height)) {
      warning("Buffer should not exceed ½ of any canvas width or height.")
      return(NA)
    }
  }

  # Helper functions
  remove_consecutive_duplicates <- function(stroke_points) {
    if(identical(NA, stroke_points) || is.null(stroke_points)) {
      stroke_points
    } else {
      stroke_points |>
        dplyr::filter_out(dplyr::lag(.x)==.x & dplyr::lag(.y)==.y)
    }
  }

  clip2canvas <- function(stroke_points, xmax, ymax) {
    stroke_points |>
      dplyr::mutate(.x = ifelse(.x<0, 0, .x)) |>
      dplyr::mutate(.y = ifelse(.y<0, 0, .y)) |>
      dplyr::mutate(.x = ifelse(.x>xmax, xmax, .x)) |>
      dplyr::mutate(.y = ifelse(.y>ymax, ymax, .y))
  }

  buff_if_point <- function(stroke_points, buffer=1, xmax, ymax) {
    # stroke_points must be tibble of .index, .x and .y
    # This function assumes no consecutive duplicate rows
    # If buffer is 0 - return empty tibble

    if(identical(NA, stroke_points) || is.null(stroke_points)) {
      stroke_points # return NA or NULL
    } else if(nrow(stroke_points) > 1) {
      stroke_points # return .. it is not a point
    } else if(buffer == 0) {
      NULL
    } else { # ..return buffered point
      x0 <- stroke_points[[1,'.x']] # pull coordinate values
      y0 <- stroke_points[[1,'.y']] # and create new tibble
      tibble::tibble(
        .x = c(x0-buffer, x0-buffer, x0+buffer, x0+buffer),
        .y = c(y0-buffer, y0+buffer, y0+buffer, y0-buffer)
      ) |>
        clip2canvas(xmax, ymax)

      # We need to reconsider clip2canvas
      # ..should we 'intersect' with canvas bounding box instead?
      # Are there situations where clip2canvas produces new
      # polygons witth no area?
    }
  }

  close_polygon <- function(stroke_points) {
    if(identical(stroke_points, NA) ||
      is.null(stroke_points) ||
      nrow(stroke_points)==0) {
        stroke_points # return NA or NULL
    } else if(nrow(stroke_points)==1) {
      stroke_points # it's a point - just return it for later buffering
    } else {
      if(identical(stroke_points[1,], stroke_points[nrow(stroke_points),])) {
        stroke_points # Already closed
      } else {
        dplyr::bind_rows(stroke_points, stroke_points[1,]) # Close it!
      }
    }
  }

  cast_all_to_polygon <- function(sf_obj, buffer=10) {
    type = as.character(sf::st_geometry_type(sf_obj))

    if(type == "POLYGON") {
      return(sf_obj)
    }
    if (type == "LINESTRING") {
      return(sf::st_buffer(sf_obj, buffer, nQuadSegs = 2))
    }
    if (type == "MULTIPOLYGON") {
      return(sf_obj |> sf::st_buffer(dist=1) |> sf::st_union() |> sf::st_make_valid() |> sf::st_cast("POLYGON"))
    }
  }

  # mirai::daemons(n)
  # map(in_parallel(...))
  # ?? should we implement this?

  # This is the main loop of this function:
  # Look at each pain drawing in turn, then at each stroke
  # in turn
  pdr_list <- pdr_list |> purrr::map(\(a_drawing) {
    tmp <- a_drawing$.points
    if (buffer == -1) {
      use_buffer = a_drawing$.strokes$.tool_width * 0.5
    } else {
      use_buffer = buffer
    }

    # Looking at a single pain drawing:
    # First group and split by stroke index, to make a list where each element is a stroke. Then remove any sequential duplicates, buffer any points, close each as a polygon, then convert to matrix, remove any empty strokes (shouldn't happen actually) and finally convert to an sf::sfc_polygon object..
    tmp <- tmp |>
      dplyr::group_by(.index) |>
      dplyr::group_split(.keep=FALSE) |>
      purrr::map(~remove_consecutive_duplicates(.x)) |>
      purrr::map(~buff_if_point(.x, 1, a_drawing$.width, a_drawing$.height)) |>
      purrr::discard(~is.null(.x)) |>
      purrr::map(~close_polygon(.x)) |>
      purrr::map(~as.matrix(.x, ncol=2,byrow=TRUE)) |>
      purrr::keep(~nrow(.x)>1) |>
      purrr::map(~sf::st_polygon(list(.x))) |>
      purrr::map(~sf::st_make_valid(.x))
    # ..then buffer with .tool_width or user provided constant and cast everything to POLYGON
    tmp <- purrr::map2(.x=tmp, .y=use_buffer, ~ sf::st_buffer(.x, .y))
    tmp <- purrr::map2(.x=tmp, .y=use_buffer, ~cast_all_to_polygon(.x, .y))
    tmp <- tmp |> sf::st_sfc()

    if(length(tmp)==0) { tmp <- NA }
    a_drawing$.polygons <- tmp
    a_drawing # map-return
  })

  return(pdr_list)
  }
