#' Trim spray pain drawing to tempalte
#'
#' This function trims a spray pain drawing (png file) to a template (png) file.
#' This is useful in a number of scenarios, e.g. to eliminate spray outside the outline of the underlying background image or
#' to limit the spray pain drawing to the subset within a particular anatomical region.
#'
#' @returns A new png spray pain drawing
#'
#' @export
#' @examples
pdr_spray_trim <- function(png_files, template) {
  if (!fs::is_file(template) || fs::is_file_empty(template)) {
    stop("Provide a single valid png file path as 'template'")
  }
  if (!all(fs::is_file(template)) || any(fs::is_file_empty(template))) {
    stop("Provide a vector of valid and non-empty png file paths as 'png'")
  }

  if (!is.character(png_files) | length(png_files < 1)) {
    stop("Provide a vector of one or more file names in 'png_files'")
  }

  # png_files is (supposed to be) a vector of files, so read them into memory
  png_data <- list()

  for (i in seq_along(png_files)) {
    if (fs::is_file(i) && !fs::is_file_empty(i)) {
      png_data <- append(
        png_data,
        tryCatch(
          expr = {
            png::readPNG(png_files[i])
          },
          error = \(e) {
            NA
          }
        )
      )
    }
  }

  # CHECK THAT PNG_FILES AND TEMPLATE HAVE SAME DIMENSIONS FOR X AND Y
  png_data <- png_data |>
    purrr::keep(\(png_mtx) {
      dim(png_mtx)[1:2] == dim(template)[1:2]
    })

  # If png_files contained valid png data, multiply each png data sets alpha channel by the alpha channel of the template png
  if (length(png_data > 0)) {
    png_data |>
      purrr::map(\(png_mtx) {
        png_mtx[,,, 4] <- png_mtx[,,, 4] * template[,,, 4]
        png_mtx
      })
    tryCatch(
      expr = {
        png::writePNG(png_mtx, target = "/tmp/NEWFILE.png")
      },
      error = \(e) {
        NA
      }
    )
  }
}
