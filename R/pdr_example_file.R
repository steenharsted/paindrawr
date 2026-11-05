#' Local absolute path to example files
#'
#' This function facilitates users' access to example files
#' such as e.g. background images, stored in the inst/extdata
#' folder of the package.
#'
#' This function does not take a pdr_list or pdr_tibble as input,
#' nor return one as output (see [pdr_match()]).
#'
#' @param path File name or path to example file in inst/extdata
#'
#' @returns An absolute file path to the specified file.
#' In case no file is specified, a vector of file names from
#' the folder, is returned.
#'
#' @export
#' @examples
#' # Get a vector of files in the _example files_ folder
#' pdr_example_file()
#'
#' # Get the absolute path to a file in the _example files_
#' # folder
#' pdr_example_data[1, ] |>
#'   pdr_plot_xy(background_image = pdr_example_file("bg_full_body.png"))
pdr_example_file <- function(path = NULL) {
  # This function provides users with easy access to example
  # data stored in the inst/extdata folder
  if (is.null(path)) {
    dir(system.file("extdata", package = "paindrawr"))
  } else {
    system.file("extdata", path, package = "paindrawr", mustWork = TRUE)
  }
}
