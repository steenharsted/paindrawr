# Synthetic drawings (3 x 4 px) with a known stencil; the stencil has 1s in
# the left column only, so its inverse has 1s everywhere else.
make_stencil_pd <- function(n = 2) {
  stencil <- matrix(0, 3, 4)
  stencil[, 1] <- 1
  tibble::tibble(
    id = as.character(seq_len(n)),
    pdr_data = lapply(seq_len(n), \(i) {
      list(
        .id = as.character(i),
        .width = 4,
        .height = 3,
        .rgba = array(0.5, dim = c(3, 4, 4)),
        .stencil = stencil,
        .stencil_inverted = 1 - stencil
      )
    })
  )
}

stencil_pd <- make_stencil_pd()
bg_png <- pdr_example_file("bg_full_body.png")

layer_geoms <- function(p) {
  vapply(p$layers, \(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)
}
# Raster of layer i as a 4 x height x width matrix of 0-255 channel values
raster_cols <- function(p, i = 1) {
  grDevices::col2rgb(p$layers[[i]]$geom_params$raster, alpha = TRUE)
}

test_that("returns a ggplot with one raster layer per drawing", {
  p <- pdr_plot_stencil(stencil_pd)

  expect_s3_class(p, "ggplot")
  expect_equal(layer_geoms(p), rep("GeomRasterAnn", 2))
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2)
})

test_that("stencil is drawn as a red mask with the stencil as alpha", {
  cols <- raster_cols(pdr_plot_stencil(stencil_pd))
  # Raster is stored row-wise, so the left column is every 4th cell
  left <- rep(c(TRUE, FALSE, FALSE, FALSE), 3)

  expect_true(all(cols["red", ] == 255))
  expect_true(all(cols["green", ] == 0))
  expect_true(all(cols["blue", ] == 0))
  expect_true(all(cols["alpha", left] == 255))
  expect_true(all(cols["alpha", !left] == 0))
})

test_that("invert = TRUE plots the inverted stencil", {
  cols <- raster_cols(pdr_plot_stencil(stencil_pd, invert = TRUE))
  left <- rep(c(TRUE, FALSE, FALSE, FALSE), 3)

  expect_true(all(cols["alpha", left] == 0))
  expect_true(all(cols["alpha", !left] == 255))
})

test_that("the original .rgba is not used for the plotted colours", {
  # .rgba is 0.5 grey everywhere, none of that should reach the plot
  cols <- raster_cols(pdr_plot_stencil(stencil_pd))

  expect_false(any(cols["green", ] == 128))
})

test_that("input data is not modified", {
  before <- stencil_pd
  pdr_plot_stencil(stencil_pd, invert = TRUE)

  expect_equal(stencil_pd, before)
})

test_that("a custom pdr_col name works", {
  # The tibble must still have a pdr_data column to count as a pdr_tibble
  with_copy <- dplyr::mutate(stencil_pd, my_col = pdr_data)

  expect_s3_class(pdr_plot_stencil(with_copy, pdr_col = my_col), "ggplot")
  expect_error(pdr_plot_stencil(stencil_pd, pdr_col = my_col))
})

test_that("ncol and nrow set the facet layout", {
  pd <- make_stencil_pd(4)

  layout <- ggplot2::ggplot_build(pdr_plot_stencil(pd, ncol = 2))$layout$layout
  expect_equal(c(max(layout$COL), max(layout$ROW)), c(2, 2))

  layout <- ggplot2::ggplot_build(pdr_plot_stencil(pd, nrow = 1))$layout$layout
  expect_equal(c(max(layout$COL), max(layout$ROW)), c(4, 1))
})

test_that("background image adds a layer per drawing beneath the stencils", {
  p <- pdr_plot_stencil(stencil_pd, background_image = bg_png)

  expect_equal(layer_geoms(p), rep("GeomRasterAnn", 4))
  expect_equal(
    p$layers[[1]]$geom_params$raster,
    p$layers[[2]]$geom_params$raster
  )
})

test_that("background image can be a path or a png array", {
  from_path <- pdr_plot_stencil(stencil_pd, background_image = bg_png)
  from_array <- pdr_plot_stencil(
    stencil_pd,
    background_image = png::readPNG(bg_png)
  )

  expect_equal(
    from_path$layers[[1]]$geom_params$raster,
    from_array$layers[[1]]$geom_params$raster
  )
})

test_that("invalid background images give informative errors", {
  expect_error(
    pdr_plot_stencil(stencil_pd, background_image = "does_not_exist.png"),
    "does not exist"
  )

  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_error(
    pdr_plot_stencil(stencil_pd, background_image = txt),
    "PNG file"
  )

  expect_error(
    pdr_plot_stencil(stencil_pd, background_image = 1:3),
    "file path to a PNG image"
  )
})

test_that("non-pdr_tibble input errors", {
  expect_error(pdr_plot_stencil("not a tibble"), "should be pdr_tibble")
  expect_error(pdr_plot_stencil(data.frame(x = 1)), "should be pdr_tibble")
})

test_that("drawings without .stencil error", {
  no_stencil <- stencil_pd
  no_stencil$pdr_data <- lapply(no_stencil$pdr_data, \(d) {
    d$.stencil <- NULL
    d$.stencil_inverted <- NULL
    d
  })

  expect_error(pdr_plot_stencil(no_stencil))
})

test_that("works end to end with pdr_add_rgba() and pdr_add_stencil()", {
  x <- suppressMessages(
    pdr_example_data[1:2, ] |>
      dplyr::mutate(
        pdr_data = pdr_add_rgba(pdr_data),
        pdr_data = pdr_add_stencil(pdr_data, threshold = 0.5)
      )
  )

  p <- pdr_plot_stencil(x)
  expect_s3_class(p, "ggplot")
  expect_equal(dim(p$layers[[1]]$geom_params$raster), c(500, 450))

  # Stencil 1 = undrawn area, so the plotted mask is opaque where nothing is drawn
  alpha <- grDevices::col2rgb(p$layers[[1]]$geom_params$raster, alpha = TRUE)[
    "alpha",
  ]
  expect_equal(
    mean(alpha == 255),
    mean(x$pdr_data[[1]]$.stencil == 1)
  )
  expect_s3_class(pdr_plot_stencil(x, invert = TRUE), "ggplot")
})
