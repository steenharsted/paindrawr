# SONs test file for polygon handling

library(tidyverse)
library(paindrawr)
library(here)

##### First load and prepare the MIRD data 'd'
# Get the raw csv data as exported from MIRD
d <- read_csv2(here("R_sandbox/paindrawing_coords.csv"))

# For debugging purposes -- lets keep it fast
d <- d[1:25,]

# Convert custom/legacy MIRD pain drawing format to paindrawr format
d <- d |> mutate(pdr_data = pdr_mird2pdr(paindrawing, app="eldorado"))
d <- d |> dplyr::select(-paindrawing)
d[8,] |> mutate(pdr_data = pdr_add_polygons(pdr_data, buffer=5))
