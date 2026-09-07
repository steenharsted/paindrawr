#' Add an RGBA data set from polygons
#'
#' @param paindrawr_data
#' @param invert
#'
#' @returns
#'
#' @export
#' @examples
pdr_add_poly_rgba <- function(paindrawr_data, invert=FALSE) {
  
  one <- 1
  zero = 0
  if(invert) {
    one <- 0
    zero = 1
  }

  result <- paindrawr_data |> 
    purrr::map(\(e) {
      xmax = e$.width
      ymax = e$.height

      v <- terra::vect(e$.polygons)

      r <- terra::rast(
        nrows = ymax,
        ncols = xmax,
        xmin = 0,
        xmax = xmax,
        ymin = 0,
        ymax = ymax
      )
      
      r <- terra::rasterize(
        v, 
        r,
        field = rep(one, length(v))
      )
      
      tmp <- terra::as.matrix(r, wide = TRUE)
      tmp[is.na(tmp)] <- zero
      storage.mode(tmp) <- "numeric"
      tmp
    })

  result # return
}