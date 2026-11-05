library(devtools)

load_all()

pdr_example_data[[1]]

my_data <- pdr_example_tibble

my_data

my_data <- my_data |> dplyr::mutate(id = pdr_get_info(pdr_data, ".id"))

my_data

my_data[1, ] |>
  pdr_plot_drawing(background = "inst/extdata/mird_body_background.png")

my_data <- my_data |>
  dplyr::mutate(flipped_pdr_data = pdr_modify(pdr_data, ops = "flipy"))

my_data

my_data[1, ] |>
  pdr_plot_drawing(
    flipped_pdr_data,
    background = "inst/extdata/mird_body_background.png"
  )

my_data <- my_data |>
  dplyr::mutate(pdr_data = flipped_pdr_data) |>
  dplyr::select(-flipped_pdr_data)

my_data[1, ] |>
  pdr_plot_drawing(
    pdr_data,
    background = "inst/extdata/mird_body_background.png",
    type = "polygon"
  )

my_data <- my_data |> dplyr::mutate(total_area = pdr_poly_areas(pdr_data))

my_data

my_data <- my_data |>
  dplyr::mutate(total_area = pdr_poly_areas(pdr_data, by = "strokes"))

my_data

anatomy <- tibble::tibble(pdr_data = pdr_example_anatomy) |>
  mutate(id = pdr_get_info(pdr_data, ".id"))

anatomy |>
  pdr_plot_drawing(
    pdr_data,
    background = "inst/extdata/mird_body_background.png"
  )

tibble(pdr_data = pdr_implode(pdr_example_anatomy)) |>
  pdr_plot_drawing(
    pdr_data,
    background = "inst/extdata/mird_body_background.png"
  )

lower_back <-
  tibble(pdr_data = pdr_example_anatomy) |>
  mutate(anatomy_region = pdr_get_info(pdr_data, ".id")) |>
  filter(stringr::str_detect(
    anatomy_region,
    "(Mid_back_bottom)|(Back.+(lowerback|buttock))"
  )) |>
  pull(pdr_data) |>
  pdr_implode()
tibble(pdr_data = lower_back) |>
  mutate(pdr_data = pdr_modify(pdr_data, "merge_edges")) |>
  pdr_plot_drawing(
    type = "polygon",
    background_image = here::here("inst", "extdata", "mird_body_background.png")
  )
