library(tidyverse)
library(devtools)

load_all()

pd <- pdr_example_data

# Saving rgba arrays in two different formats
pd <- pd |>
  mutate(
    rgbas = pdr_add_rgba(pdr_data, rgba_only = TRUE),
    pdr_data = pdr_add_rgba(pdr_data)
  )

# Giving a list of rgba arrays
# works!
pd |>
  mutate(
    intensity_mean = pdr_summarize_alpha_intensity(rgbas), # summary_stat defaults to mean
    intensity_max = pdr_summarize_alpha_intensity(rgbas, summary_stat = "max"),
    intensity_min = pdr_summarize_alpha_intensity(rgbas, "min"),
    area = pdr_summarize_alpha_area(rgbas)
  )


# Giving a list of paindrawing data lists WITH a list of .rgba arrays
pd |>
  mutate(
    intensity_mean = pdr_summarize_alpha_intensity(pdr_data),
    intensity_max = pdr_summarize_alpha_intensity(pdr_data, "max"),
    area = pdr_summarize_alpha_area(pdr_data)
  )


# Given a single raw rgba array
pdr_example_data$pdr_data[[1]] |>
  pdr_add_rgba(rgba_only = TRUE) |>
  pdr_summarize_alpha_intensity()


pdr_example_data$pdr_data[[1]] |>
  pdr_add_rgba(rgba_only = TRUE) |>
  pdr_summarize_alpha_area()
