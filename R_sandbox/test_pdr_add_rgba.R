library(devtools)
library(tidyverse)

load_all()

# Load sample data
pd <- pdr_example_data$pdr_data

pd

# multi pdr list in multi pdr list with .rgba added out
pdr_add_rgba(pdr_list = pdr_example_data$pdr_data[1])

# multi pdr list in multi list of rgba arrays out
pdr_add_rgba(
  pdr_list = ,
  rgba_only = TRUE
)

# single pdr named list in single pdr named list out with .rgba added
pd[[1]] |> pdr_add_rgba()

## Intended use
pdr_tib <- pdr_example_data |>
  mutate(new_pdr_data = pdr_add_rgba(paindrawr_data = pdr_data))
pdr_tib

pdr_tib <- pdr_tib |>
  mutate(
    .rgba_list = pdr_add_rgba(paindrawr_data = pdr_data, rgba_only = TRUE)
  )
pdr_tib


## Plot an RGBA array
grid::grid.newpage()
pdr_tib$.rgba_list[[2]]$.rgba |> grid::grid.raster()

pdr_tib[2, ] |> pdr_plot_drawing()


# Test bug with only one row in tibble
pdr_tib |>
  slice_sample(n = 1) |>
  mutate(
    new = pdr_add_rgba(pdr_data),
    new2 = pdr_add_rgba(pdr_data, rgba_only = TRUE)
  )
