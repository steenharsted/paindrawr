pd_geom_chull <- function(pd) {
  # Run data sanity check ... to be completed
  # This needs to be re-written to reflect the list of 3x tibbles data structure of pd
  # require(purrr)
  # pd |> map(function(pd_schematic)
  # { # This chunk must return a valid pd element, i.e. a paindrawing schematic
  #   pd_schematic$s$p <- pd_schematic$s$p |> map(function(p)
  #   { # This chunk must return a valid p element i.e an n⏺2 matrix
  #     if(length(p>6)) {
  #       vertices_on_chull <- chull(p)
  #       p <- p[vertices_on_chull,] # Retain those row/vertices which are in chull
  #     } else {
  #       p
  #     } # end if
  #   }) # end map
  #   pd_schematic
  # }) # end map
}
