# This file creates sample data coming from different imports
# Import import will have at least 2 valid, 1 empty, and 1 NA
# The purpose of the tets-data is to tests that we handle
# these types of data as intended
# Rules are:
# NA in NA out
# Empty drawings are valid, and are just considered as not indicating pain

# Data from json
json_files <- list.files(
  "tests/testthat/fixtures/json",
  pattern = "json$",
  full.names = TRUE
)

## add NA
json_files[4] <- NA

# Data from png
png_files <- list.files(
  "tests/testthat/fixtures/png",
  pattern = "png$",
  full.names = TRUE
)

## add NA
png_files[9] <- NA

## Data from Mird
mird_data <- read.csv("tests/testthat/fixtures/mird/mird_test.csv")


paindrawr::pdr_import_png(png_files)
paindrawr::pdr_import_json(json_files)
mird_data |>
  dplyr::mutate(
    pdr_data = pdr_mird2pdr("paindrawing_LBP")
  )


mird_data[4, ] |>
  as_tibble() |>
  mutate(
    pdr_data = pdr_mird2pdr(paindrawing_LBP)
  )

my_mird[2, ]$pdr_data
