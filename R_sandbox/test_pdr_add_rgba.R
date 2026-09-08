library(devtools)
library(tidyverse)

load_all()

# Load sample data
pd <- pdr_example_data$pdr_data


# multi pdr list in multi pdr list with .rgba added out
pdr_add_rgba(paindrawr_data = pd[1])

# multi pdr list in multi list of rgba arrays out
pdr_add_rgba(
  paindrawr_data = pd,
  rgba_only = TRUE
)

# single pdr named list in single pdr named list out with .rgba added
pd[[1]] |> pdr_add_rgba()

## Intended use
pdr_tib <- pdr_example_data |>
  mutate(new_pdr_data = pdr_add_rgba(paindrawr_data = pdr_data))
pdr_tib

pdr_tib <- pdr_tib|>
  mutate(
    .rgba = pdr_add_rgba(paindrawr_data = pdr_data, rgba_only = TRUE)
  )
pdr_tib


## Plot the RGBA arrays
grid::grid.newpage()
pd$.rgba[[2]] |> grid::grid.raster()


list_with_.rgba <- pdr_example_data[[1]] |> pdr_add_rgba()

grid::grid.newpage()
list_with_.rgba$.rgba |> grid::grid.raster()


pd <- pd |>
  dplyr::mutate(
    .rgba = purrr::map(pdr_data, \(x) pdr_add_rgba(x, rgba_only = TRUE))
  )

pd |>
  dplyr::mutate(
    rgba_arrays = pdr_add_rgba(pdr_data, rgba_only = TRUE)
  )
