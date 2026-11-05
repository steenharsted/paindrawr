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
    pdr_data = pdr_add_stencil(pdr_data)
  )

# Plot stencil
my_pdr_tib |>
  pdr_plot_stencil(
    pdr_col = pdr_data
  )

# function pdr_collapse_stencils
# Intended for use inside summarise
# Take a pdr_list containing stencils
# Dimensions must be identical
# Use .stencil and add the stencils together
# Cap values at 1 (we can have overlapping stencils)
# Create an .stencil_inverted from the combined stencil
# Return a pdr_list with a single .stencil, .stencil_inverted, height, and width, id? maybe optional

my_pdr_tib[["pdr_data"]]
