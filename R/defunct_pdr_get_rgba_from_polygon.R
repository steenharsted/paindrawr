#' Add an RGBA data set from polygons
#'
#' @param pdr_list A valid pain drawing list-col -- see `pdr_check_data()` for details
#' @param invert
#'
#' @returns
#'
#' @export
#' @examples







# pdr_get_rgba_from_polygon <- function(pdr_list, invert=FALSE) {

#   one <- 1
#   zero = 0
#   if(invert) {
#     one <- 0
#     zero = 1
#   }

#   result <- pdr_list |>
#     purrr::map(\(e) {
#       xmax = e$.width
#       ymax = e$.height

#       v <- terra::vect(e$.polygons)

#       r <- terra::rast(
#         nrows = ymax,
#         ncols = xmax,
#         xmin = 0,
#         xmax = xmax,
#         ymin = 0,
#         ymax = ymax
#       )

#       r <- terra::rasterize(
#         v,
#         r,
#         field = rep(one, length(v))
#       )

#       tmp <- terra::as.matrix(r, wide = TRUE)
#       tmp[is.na(tmp)] <- zero
#       storage.mode(tmp) <- "numeric"
#       e$.rgba <- tmp
#       e
#     })

#   result # return
# }
