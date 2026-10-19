#' Convert MIRD pain drawings to pdr_tibble 
#' 
#' MIRD exports pain drawings in a custom format like:
#' (,1,2,5,4,),(,8,8,7,7,)
#' which corresponds to:
#'  x | y | index
#' ---|---|-------
#'  1 | 2 | 1
#'  5 | 4 | 1
#'  8 | 8 | 2
#'  7 | 7 | 2
#' 
#' This functions takes a vector of length n of such MIRD
#' strings and converts it into a list of length n of valid
#' paindrawr data list col.
#' 
#' The intended use case, is a data frame (tibble) of MIRD
#' data including pain drawing data. A new paindrawr data
#' list cloumn can be generate with a simple mutate() call.
#'
#' Each of the parameters supplied must be either of the 
#' same length as MIRD pain drawing vector or of length 1,
#' in which case it will be repeated.
#' 
#' @param mird_pd A vector of MIRD pain drawings 
#' @param id A unique id for each drawing (default: 1:length(mird_pd)) 
#' @param file Defaults to ""
#' @param version Defaults to ""
#' @param width of canvas -- Defaults to 450
#' @param height of canvas -- Defaults to 500
#' @param units Defaults to "px"
#' @param timestamp Defaults to ""
#' @param app Defaults to ""
#' @param tool Defaults to "pen"
#' @param draw_input_type Defaults to "mouse"
#' @param tool_width Defaults to "1"
#' @param point_density Defaults to "1"
#' @param spray_radius Defaults to "1"
#' @param alpha Defaults to "1.0"
#' @param color Defaults to "#ff0000"
#'
#' @returns A valid pain drawing data list column
#'
#' @export
#' @examples
#' readr::read_csv("data-raw/short_demo_data_with_NA.csv") |> mutate(pdr_data = pdr_mird2pdr(paindrawing_LBP, id=id))
#' 
pdr_mird2pdr <- function(
  mird_pd,
  id = 1:length(mird_pd),
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
  flipy=TRUE) {
  
  #### Sanity check
  if(!is.character(mird_pd)) {
    warning("In'pdr_mird2pdr()' parameter 'mird_pd' is wrong type -- expected character vector. ")
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
    if (is.na(ms) || !is.character(ms) || stringr::str_length(ms) < 7) { # "(,x,y,)" is length 7
      return(NA)
      #return(tibble::tibble(.index = as.integer(), .x=as.integer(), .y=as.integer()))
    } else {
      # Simplify notation
      # Go from (,1,2,5,4,),(,8,8,7,7,)
      # to          1,2,5,4;8,8,7,7
      ms <- stringr::str_replace_all(ms, ",\\),\\(,", ";") # ,),(,  --> ;
      ms <- stringr::str_replace_all(ms, "\\(,", "")       # (,     -->
      ms <- stringr::str_replace_all(ms, ",\\)", "")       # ),     -->

      points <- stringr::str_split(ms, ";", simplify = TRUE) # split to chr [1, 1:2] "1,2,5,4" "8,8,7,7"
      points <- stringr::str_split(points, ",") # split to list [[1]] [1] "1" "2" "5" "4" [[2]] [1] "8" "8" "7" "7"
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
    # This function creates a new .strokes tibble from a 
    # .points tibble
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
  
  #### This does the actual conversion from MIRD string to
  # valid pain drawing data list column.
  # Pull the MIRD string as a vector, then map it to a list,
  # where each element is a list of named elements (.id, 
  # .file, ..etc) -- most are parameters passed when calling
  # this function (e.g. .width and .height), but some are 
  # replaced by values from the MIRD data if the parameter 
  # is not specified (id, file and timestamp), and lastly 
  # the .points and .strokes are calculated from the MIRD 
  # pain drawing string strings
  pdr_list <- mird_pd |> 
    purrr::map(\(a_mird_pd) {
      points <- convert_mird_to_tibble(a_mird_pd) # tibble
      strokes <- convert_points_to_strokes(
        p = points, 
        tool = tool,
        draw_input_type = draw_input_type,
        tool_width = tool_width,
        point_density = point_density,
        spray_radius = spray_radius,
        alpha = alpha,
        color = color
      )
      list(        
        .id = id,
        .file = file,
        .version = version,
        .width = width,
        .height = height,
        .units = units,
        .timestamp = timestamp,
        .app = app,
        .points = points,
        .strokes = strokes
      )
    })
  
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
    })
  
  # if(flipy) {
  #   pdr_list <- pdr_list |> pdr_modify_points(ops="flipy")
  # }
  
  # Return this
  pdr_list
}