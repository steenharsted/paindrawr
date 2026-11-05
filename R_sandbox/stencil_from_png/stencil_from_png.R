library(devtools)
library(tidyverse)
load_all()


pngs <- list.files(
  "R_sandbox/stencil_from_png",
  pattern = "\\.png",
  full.names = TRUE
)

my_pdr_tib <- pdr_import_png(
  files = pngs
)


# Sanity check
## Height and width should be identical
## Here we include the orginial image.
my_pdr_tib <- my_pdr_tib |>
  mutate(
    height = pdr_get_info(pdr_data, id1 = ".height"),
    width = pdr_get_info(pdr_data, id1 = ".width")
  )


# Drop the full image
# Not needed after sanity check
my_pdr_tib <- my_pdr_tib |>
  filter_out(id == "bg_feet")


# Plots rgbas
my_pdr_tib |>
  pdr_plot_rgba(
    include_id = TRUE,
    background_image = pdr_example_file("bg_feet.png")
  )

# Make stencils
my_pdr_tib <- my_pdr_tib |>
  mutate(
    pdr_data = pdr_add_stencil(
      pdr_list = pdr_data,
      threshold = 0, # should this not be default??
      by = ">" # should this not be default??
    )
  )

# Plot stencil
my_pdr_tib |>
  pdr_plot_stencil(
    pdr_col = pdr_data
  )


# Try it: collapse all nails into one stencil
my_collapsed <- my_pdr_tib |>
  summarise(
    id = "nails",
    pdr_data = pdr_collapse_stencils(pdr_data, id = "nails")
  )

my_collapsed |>
  pdr_plot_stencil(
    pdr_col = pdr_data,
    background_image = pdr_example_file("bg_feet.png")
  )


# with group by
my_group_collapsed <- my_pdr_tib |>
  mutate(
    toe = if_else(str_detect(id, "1"), "Big toes", "Small toes")
  ) |>
  group_by(toe) |>
  summarise(
    pdr_data = pdr_collapse_stencils(pdr_data)
  )

my_group_collapsed |>
  pdr_plot_stencil(
    pdr_col = pdr_data,
    background_image = pdr_example_file("bg_feet.png"),
    include_id = TRUE
  )


my_group_collapsed |>
  pdr_plot_rgba(
    pdr_col = pdr_data,
    background_image = pdr_example_file("bg_feet.png"),
    include_id = TRUE
  )
