# Prepare the `pdr_example_palmar` dataset from the files in data-raw/palmar/
# JSON files are sanitised in place first (see sanitize_pdr_json.R), and file
# path and timestamp are removed from the drawings, so no identifying metadata
# is stored in the repository or package. Participants without a drawing are
# kept with a missing drawing.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
devtools::load_all()
source("data-raw/sanitize_pdr_json.R")

files <- list.files("data-raw/palmar", pattern = "\\.json$", full.names = TRUE)
sanitize_pdr_json(files)

drawings <- pdr_import_json(files) |>
  mutate(
    # Id is the leading number of the file name
    id = map_int(pdr_data, \(d) as.integer(str_extract(basename(d$.file), "^[0-9]+"))),
    pdr_data = map(pdr_data, \(d) {
      d$.id <- as.character(as.integer(str_extract(basename(d$.file), "^[0-9]+")))
      d$.file <- NA_character_
      d$.timestamp <- NA_character_
      d
    })
  )

pdr_example_palmar <- read_csv("data-raw/palmar/palmar.csv", show_col_types = FALSE) |>
  left_join(drawings, by = "id") |>
  rename(palmar = pdr_data) |>
  mutate(
    distance = factor(distance, levels = c("10 centimeters", "1 meter")),
    tool = factor(tool)
  ) |>
  select(id, palmar, tool, distance)

usethis::use_data(pdr_example_palmar, overwrite = TRUE, compress = "bzip2")
