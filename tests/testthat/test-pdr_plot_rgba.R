json_fixture <- function(n) {
  test_path("fixtures", "json", paste0(n, "_palmar.json"))
}

# Example data contains pen strokes (450 x 500); JSON fixtures contain spray
# strokes (624 x 720).
pen_rgba <- function(rows = 1:2) {
  pdr_example_data[rows, ] |>
    dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
}
spray_rgba <- function(n = 1) {
  pdr_import_json(json_fixture(n)) |>
    dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
}
bg_png <- pdr_example_file("bg_full_body.png")

layer_geoms <- function(p) {
  vapply(p$layers, \(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)
}
raster_of <- function(layer) layer$geom_params$raster
n_panels <- function(p) nrow(ggplot2::ggplot_build(p)$layout$layout)

test_that("returns a ggplot with one raster layer per drawing", {
  p <- pdr_plot_rgba(pen_rgba(1:3))

  expect_s3_class(p, "ggplot")
  expect_equal(layer_geoms(p), rep("GeomRasterAnn", 3))
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(n_panels(p), 3)
})

test_that("rasters keep the .rgba dimensions", {
  p <- pdr_plot_rgba(pen_rgba(1))
  r <- raster_of(p$layers[[1]])

  expect_equal(dim(r), c(500, 450))
})

test_that("coordinates are fixed to the canvas size", {
  p <- pdr_plot_rgba(spray_rgba(1))

  expect_equal(p$coordinates$ratio, 1)
  expect_equal(p$coordinates$limits$x, c(0, 624))
  expect_equal(p$coordinates$limits$y, c(0, 720))
})

test_that("a custom pdr_col name works", {
  renamed <- dplyr::rename(pen_rgba(), my_col = pdr_data)

  expect_s3_class(pdr_plot_rgba(renamed, pdr_col = my_col), "ggplot")
  expect_error(pdr_plot_rgba(pen_rgba(), pdr_col = my_col))
})

test_that("missing .rgba errors with a hint", {
  expect_error(pdr_plot_rgba(pdr_example_data[1:2, ]), "pdr_add_rgba")
})

test_that("only empty .rgba gives an error", {
  empty <- pen_rgba()
  empty$pdr_data <- list(
    list(.rgba = numeric(0)),
    list(.rgba = numeric(0))
  )

  expect_error(pdr_plot_rgba(empty), "No non-missing .rgba")
})

test_that("drawings with missing .rgba are skipped", {
  with_gap <- pen_rgba(1:3)
  with_gap$pdr_data[[2]]$.rgba <- NULL

  p <- pdr_plot_rgba(with_gap)

  expect_equal(layer_geoms(p), rep("GeomRasterAnn", 2))
  expect_equal(n_panels(p), 2)
})

test_that("include_id controls facet strip labels", {
  hidden <- pdr_plot_rgba(pen_rgba())
  shown <- pdr_plot_rgba(pen_rgba(), include_id = TRUE)

  expect_s3_class(hidden$theme$strip.text, "ggplot2::element_blank")
  expect_false(inherits(shown$theme$strip.text, "ggplot2::element_blank"))
})

test_that("facet panels are labelled with the drawing .id", {
  x <- pen_rgba()
  p <- pdr_plot_rgba(x, include_id = TRUE)
  ids <- as.character(vapply(x$pdr_data, \(d) as.character(d$.id), ""))

  expect_equal(as.character(ggplot2::ggplot_build(p)$layout$layout$.id), ids)
})

test_that("ncol and nrow are passed to facet_wrap", {
  p <- pdr_plot_rgba(pen_rgba(1:4), ncol = 2)
  layout <- ggplot2::ggplot_build(p)$layout$layout
  expect_equal(max(layout$COL), 2)
  expect_equal(max(layout$ROW), 2)

  p <- pdr_plot_rgba(pen_rgba(1:4), nrow = 1)
  layout <- ggplot2::ggplot_build(p)$layout$layout
  expect_equal(max(layout$ROW), 1)
  expect_equal(max(layout$COL), 4)
})

test_that("background image adds one layer per drawing beneath the drawings", {
  p <- pdr_plot_rgba(pen_rgba(), background_image = bg_png)

  expect_equal(layer_geoms(p), rep("GeomRasterAnn", 4))
  # Backgrounds come first and are identical
  expect_equal(raster_of(p$layers[[1]]), raster_of(p$layers[[2]]))
  expect_false(identical(raster_of(p$layers[[1]]), raster_of(p$layers[[3]])))
})

test_that("background image can be a path or a png array", {
  from_path <- pdr_plot_rgba(pen_rgba(1), background_image = bg_png)
  from_array <- pdr_plot_rgba(
    pen_rgba(1),
    background_image = png::readPNG(bg_png)
  )

  expect_equal(
    raster_of(from_path$layers[[1]]),
    raster_of(from_array$layers[[1]])
  )
})

test_that("invalid background images give informative errors", {
  expect_error(
    pdr_plot_rgba(pen_rgba(), background_image = "does_not_exist.png"),
    "does not exist"
  )

  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_error(pdr_plot_rgba(pen_rgba(), background_image = txt), "PNG file")

  expect_error(
    pdr_plot_rgba(pen_rgba(), background_image = 1:3),
    "must be a PNG file path or a raster array"
  )
})

test_that("only_alpha draws drawn pixels black and others white", {
  x <- pen_rgba(1)
  rgba <- x$pdr_data[[1]]$.rgba
  r <- raster_of(pdr_plot_rgba(x, only_alpha = TRUE)$layers[[1]])
  cols <- grDevices::col2rgb(r, alpha = TRUE)

  drawn <- as.vector(t(rgba[,, 4] > 0))

  expect_true(any(drawn))
  expect_true(all(cols["alpha", drawn] == 255))
  expect_true(all(cols[1:3, drawn] == 0))
  expect_true(all(cols[1:3, !drawn] == 255))
})

test_that("only_alpha = FALSE keeps the original colours", {
  x <- pen_rgba(1)
  r <- raster_of(pdr_plot_rgba(x)$layers[[1]])

  expect_equal(r, as.raster(x$pdr_data[[1]]$.rgba))
})

test_that("input data is not modified", {
  x <- pen_rgba()
  before <- x
  pdr_plot_rgba(x, only_alpha = TRUE)

  expect_equal(x, before)
})
