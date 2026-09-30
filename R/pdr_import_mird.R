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
  color = "#ff0000") {
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
  if(!any(paindrawing_col %in% names(content))) {
    warning(paste0("The file ",csv_file," does not include a column ",paindrawing_col, "."))
    return(NA)
  }

  # Re-use (some) data from MIRD in the pdr_list column, IF
  # they are not specified by user and they are in the mird data
  if(ids=="" && any("id" %in% names(content))) { ids = content$id}
  if(file=="") { file = csv_file }
  if(timestamp=="" && any("date_of_completion" %in% names(content))) { timestamp = content$data_of_completion}

  pdr_data <- content |> dplyr::pull({{ paindrawing_col}}) |> purrr::map(\(x) {
    
  })

  pdr_mird2pdr(
    mird_string = paindrawing_col,
    ids,
    file,
    version,
    width,
    height,
    units,
    timestamp = as.character(Sys.time()),
    app,
    tool,
    draw_input_type,
    tool_width,
    point_density,
    spray_radius,
    alpha,
    color
  ) # Return this as result
}