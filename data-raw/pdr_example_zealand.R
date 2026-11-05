# Prepare the `pdr_example_zealand` dataset from the files in data-raw/zealand/
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

files <- list.files("data-raw/zealand", pattern = "\\.json$", full.names = TRUE)
sanitize_pdr_json(files)

drawings <- pdr_import_json(files) |>
  mutate(
    # Id is the leading number of the file name
    id = map_int(pdr_data, \(d) {
      as.integer(str_extract(basename(d$.file), "^[0-9]+"))
    }),
    pdr_data = map(pdr_data, \(d) {
      d$.id <- as.character(as.integer(str_extract(
        basename(d$.file),
        "^[0-9]+"
      )))
      d$.file <- NA_character_
      d$.timestamp <- NA_character_
      d
    })
  )

pdr_example_zealand <- read_csv(
  "data-raw/zealand/zealand.csv",
  show_col_types = FALSE
) |>
  left_join(drawings, by = "id") |>
  rename(zealand = pdr_data) |>
  mutate(geography = as.integer(geography)) |>
  select(id, zealand, geography)

usethis::use_data(pdr_example_zealand, overwrite = TRUE, compress = "bzip2")
