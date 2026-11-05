pd_geom_locate_overlapping_bbox <- function(pd) {
  pd$strokes |>
    group_by(id) |>
    group_modify(\(strokes_by_id) {
      strokes_i <- unique(strokes_by_id$i) # Hold the actual identifier of the discrete strokes
      n_strokes <- length(strokes_i) # How many of them there are (this may change in the while loop)
      p1 <- 1 # pointer 1
      p2 <- 2 # pointer 2

      while (p1 < n_strokes) {
        while (p2 <= n_strokes) {
          #bbox_overlap <- {strokes_by_id |> dplyr:: filter()}
        }
      }
    })
}
