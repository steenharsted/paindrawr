pd_geom_calc_bounding_box <- function(pd) {
  bbox <- pd$points |>
    dplyr::group_by(id, i) |>
    dplyr::summarize(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y))
  pd$strokes <- dplyr::full_join(pd$strokes, bbox, by = c("id", "i"))
  pd
}
