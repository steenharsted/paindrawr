
#' Convert raw pain drawing coordinate data into sf geometry
#' polygons
#'
#' @param pdr A valid pain drawing list-col -- see 
#' `pdr_check_data()` for details
#' 
#' @param buffer An integer value specifying the buffering
#' distance of point and line segment buffering. If set to 
#' 0, points and line segments will be omitted. The buffer 
#' can not be larger than 50% of either canvas diameter.
#'
#' @returns The input parameter `pdr` with a new `.polygons`
#' element added to each pain drawing element.
#' 
#'
#' @export
#' @examples
#' 
#' pdr_example_geometry |> 
#'   mutate(pdr_data = pdr_polygonize(pdr_data))
#' 
#' pdr_example_geometry |> 
#'   mutate(pdr_data = pdr_polygonize(pdr_data, buffer=0))
#' 
#' 
pdr_polygonize <- function(pdr, buffer=5) {
  # pdr is assumed to be valid pain drawing list-col data
  if(buffer < 1) { buffer = 1} # Is this what we want? Should buffer==0 be used to delete points and lines?
  buffer = abs(as.integer(buffer))

  # Creates new element .polygons in each pain drawing in pdr
  # Each .polygons is an sf object - st_multipolygon
  
  # We need to ensure than any buffering can be cropped to 
  # canvas if it exceeds it, i.e. must be < ½ canvas dimensions
  if(buffer > 0) {
    min_width <- pdr |> purrr::map_int(\(df) df$.width) |> min() 
    min_height <- pdr |> purrr::map_int(\(df) df$.height) |> min()
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

  buff_if_point <- function(stroke_points, buffer=5, xmax, ymax) {
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

  buff_if_line <- function(poly, buffer, xmax, ymax) {
    # poly must be a valid sd::st_polygon object
    # If buffer is 0 - remove the line by returning NULL

    if(sf::st_area(poly) == 0) {
      if(buffer == 0) {
        NULL
      } else {
        poly |>
          sf::st_buffer(dist=buffer, nQuadSegs = 1) |>
          sf::st_convex_hull() 
          #sf::st_crop(c(xmin=0, ymin=0,xmax=xmax,ymax=ymax))
      }
    } else {
      poly
    }
  }

  close_polygon <- function(stroke_points) {
    if(identical(stroke_points, NA) || 
      is.null(stroke_points) ||
      nrow(stroke_points)==0) {
        stroke_points # return NA or NULL
    } else if(nrow(stroke_points)==1) {
      stroke_points # return point
    } else {
      if(identical(stroke_points[1,], stroke_points[nrow(stroke_points),])) {
        stroke_points
      } else {
        dplyr::bind_rows(stroke_points, stroke_points[1,])
      }   
    }
  }


  # mirai::daemons(n)
  # map(in_parallel(...))
  # ?? should we implement this?

  # This is the main loop of this function.
  # Look at each pain drawing in turn, then at each stroke
  # in turn
  pdr <- pdr |> purrr::map(\(a_drawing) {
    tmp <- a_drawing$.points 
    # Looking at a single pain drawing:
    # First group and split by stroke index, then remove
    # any sequential duplicates, buffer any points (note 
    # that 0-buffer removes points), close each as a polygon, 
    # then convert to matrix, remove any empty strokes (e.g
    # points removed by buffer 0) and finally convert to an
    # sf::sfc_polygon object
    tmp <- tmp |> 
      dplyr::group_by(.index) |>
      dplyr::group_split(.keep=FALSE) |>
      purrr::map(~remove_consecutive_duplicates(.x)) |>
      purrr::map(~buff_if_point(.x, buffer, a_drawing$.width, a_drawing$.height)) |>
      purrr::discard(~is.null(.x)) |>
      purrr::map(~close_polygon(.x)) |>
      purrr::map(~as.matrix(.x, ncol=2,byrow=TRUE)) |>
      purrr::keep(~nrow(.x)>1) |>
      purrr::map(~sf::st_polygon(list(.x))) |>
      purrr::map(~sf::st_convex_hull(.x)) |>
      #purrr::map(~sf::st_concave_hull(.x, ratio=0.5, allow_holes=FALSE)) |>
      purrr::map(~sf::st_make_valid(.x)) |>
      sf::st_sfc()
    
    # We now need to deal with lines which have area==0. 
    # This is not as simple as checking whether there are 
    # only 2 points, as multiple points may lie on the same 
    # line, which is why why wait to deal with them until 
    # after converting to sf polygon object
    tmp <- tmp |>
      purrr::map(~buff_if_line(.x, buffer, a_drawing$.width, a_drawing$.height)) |>
      purrr::discard(~is.null(.x)) |>
      sf::st_sfc()
    
    if(length(tmp)==0) { tmp <- NA }
    a_drawing$.polygons <- tmp
    a_drawing # return at endmap
  })

  return(pdr)
  }
