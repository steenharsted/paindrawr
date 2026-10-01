#' Import MIRD file as pain drawing data
#'
#' @param file File name (path) to a MIRD export csv file which includes a pain drawing string column.
#'
#' @returns A valid pain drawing data tibble.
#'
#' @export
#' @examples
pdr_import_mird <- function(
  csv_file, 
  paindrawing_col="paindrawing_LBP",
  ids = "",
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
  alpha = as.integer(255),
  color = "#ff0000",
  pdr_data_col_name = "pdr_data") {
  #### Sanity checks
  if(!fs::file_exists(csv_file)) {
    warning(paste0("The file ",csv_file," does not exist."))
    return(NA)
  }
  if(!fs::file_size(csv_file)==0) {
    warning(paste0("The file ",csv_file," is empty."))
    return(NA)
  }

  content <- read.csv(csv_file)
  
  #### Sanity check
  if(!(paindrawing_col %in% names(content))) {
    stop(paste0("The file ",csv_file," does not include a column ",paindrawing_col, "."))
  }

  if({{pdr_data_col_name}} %in% names(content)) {
    warning(paste0("Column ",pdr_data_col_name," in data from file ",
    csv_file," will be replaced by new pain drawing data list column -- consider specifying a different column name with parameter 'pdr_data_col_name'."))
  }

  # Re-use (some) data from MIRD in the pdr_list column, IF
  # they are not specified by user and they are in the mird data
  if(ids=="" && any("id" %in% names(content))) { ids = content$id}
  if(file=="") { file = csv_file }
  if(timestamp=="" && any("date_of_completion" %in% names(content))) { timestamp = content$data_of_completion}

  #### Helper functions
    # This function is vectorized and returns a list
  # Each string in mird_string becomes an element in the list
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
  # is not specified (ids, file and timestamp), and lastly 
  # the .points and .strokes are calculated from the MIRD 
  # pain drawing string strings
  pdr_data <- content |> 
    dplyr::pull({{ paindrawing_col}}) |>
    purrr::map(\(a_string) {
      points <- convert_mird_to_tibble(a_string)
      strokes <- convert_points_to_strokes(
        p = points, 
        tool = tool,
        draw_input_type = draw_input_type,
        tool_width = tool_width,
        point_density = point_density,
        spray_radius = spray_radius,
        alpha = alpha,
        color = color,
      )
      list(        
        .id = ids,
        .file = file,
        .version = version,
        .width = width,
        .height = height,
        .units = px,
        .timestamp = timestamp,
        .app = app,
        .points = points,
        .strokes = strokes
      )
    })
  
  # Return this
  content |>
    dplyr::mutate({{pdr_data_new_col = pdr_data}})
}