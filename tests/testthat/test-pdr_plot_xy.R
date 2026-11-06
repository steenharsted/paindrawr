json_fixture <- function(n) {
  test_path("fixtures", "json", paste0(n, "_palmar.json"))
}

# JSON fixtures contain spray strokes (624 x 720); example data contains pen
# strokes (450 x 500).
spray_pd <- function(n = 1:2) pdr_import_json(vapply(n, json_fixture, ""))
pen_pd <- pdr_example_data[1:2, ]
bg_png <- pdr_example_file("bg_full_body.png")

layer_geoms <- function(p) {
  vapply(p$layers, \(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)
}

test_that("default output is a rasterized ggplot with one raster layer", {
  p <- pdr_plot_xy(pen_pd)

  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 1)
  expect_equal(layer_geoms(p), "GeomRasterAnn")
})

test_that("rasterized plot has a locked aspect ratio from canvas size", {
  p <- pdr_plot_xy(spray_pd(1))
  expect_equal(p$coordinates$ratio, 720 / 624)

  # Two drawings facet into a 2 x 1 grid -> width doubles, ratio halves
  p2 <- pdr_plot_xy(spray_pd(1:2))
  expect_equal(p2$coordinates$ratio, 720 / (624 * 2))
})

test_that("rasterize = FALSE returns a vector plot with facets", {
  p <- pdr_plot_xy(pen_pd, rasterize = FALSE)

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
  expect_true("GeomPath" %in% layer_geoms(p))
  expect_true("GeomTile" %in% layer_geoms(p))
  expect_equal(p$coordinates$limits$x, c(0, 450))
  expect_equal(p$coordinates$limits$y, c(0, 500))
})

test_that("vector plot has one facet per drawing", {
  p <- pdr_plot_xy(spray_pd(1:2), rasterize = FALSE)
  expect_setequal(unique(ggplot2::ggplot_build(p)$layout$layout$PANEL), 1:2)
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2)
})

test_that("spray strokes are expanded by point density", {
  set.seed(5521)
  p <- pdr_plot_xy(spray_pd(1), rasterize = FALSE)
  spray_layer <- p$layers[[which(layer_geoms(p) == "GeomTile")]]
  d <- spray_pd(1)$pdr_data[[1]]
  per_point <- dplyr::inner_join(d$.points, d$.strokes, by = ".index")

  expect_equal(nrow(spray_layer$data), sum(per_point$.point_density))
})

test_that("spray data is jittered randomly but reproducibly with a seed", {
  get_x <- function() {
    p <- pdr_plot_xy(spray_pd(1), rasterize = FALSE)
    p$layers[[which(layer_geoms(p) == "GeomTile")]]$data$.x
  }
  set.seed(8310)
  a <- get_x()
  set.seed(8310)
  b <- get_x()
  set.seed(1207)
  c <- get_x()

  expect_equal(a, b)
  expect_false(isTRUE(all.equal(a, c)))
})

test_that("pen-only data gives an empty spray layer, spray-only an empty pen layer", {
  pen <- pdr_plot_xy(pen_pd, rasterize = FALSE)
  spray <- pdr_plot_xy(spray_pd(1), rasterize = FALSE)

  expect_equal(
    nrow(pen$layers[[which(layer_geoms(pen) == "GeomTile")]]$data),
    0
  )
  expect_equal(nrow(pen$data), nrow(pen$data[pen$data$.tool == "pen", ]))
  expect_equal(nrow(spray$data), 0)
})

test_that("polygon argument swaps path/point layers for polygons", {
  expect_true(
    "GeomPolygon" %in%
      layer_geoms(
        pdr_plot_xy(pen_pd, rasterize = FALSE, polygon = "pen")
      )
  )
  expect_false(
    "GeomPath" %in%
      layer_geoms(
        pdr_plot_xy(pen_pd, rasterize = FALSE, polygon = "pen")
      )
  )

  spray_poly <- pdr_plot_xy(spray_pd(1), rasterize = FALSE, polygon = "spray")
  expect_false("GeomTile" %in% layer_geoms(spray_poly))
  expect_true("GeomPolygon" %in% layer_geoms(spray_poly))

  both <- pdr_plot_xy(pen_pd, rasterize = FALSE, polygon = "both")
  expect_equal(sum(layer_geoms(both) == "GeomPolygon"), 2)
})

test_that("include_id controls facet strip labels", {
  hidden <- pdr_plot_xy(pen_pd, rasterize = FALSE)
  shown <- pdr_plot_xy(pen_pd, rasterize = FALSE, include_id = TRUE)

  expect_s3_class(hidden$theme$strip.text, "ggplot2::element_blank")
  expect_false(inherits(shown$theme$strip.text, "ggplot2::element_blank"))
})

test_that("a custom pdr_col name works", {
  renamed <- dplyr::rename(pen_pd, my_col = pdr_data)

  expect_s3_class(pdr_plot_xy(renamed, pdr_col = my_col), "ggplot")
  expect_error(pdr_plot_xy(pen_pd, pdr_col = my_col))
})

test_that("method = 'file' gives the same image as 'memory'", {
  raster_of <- function(p) p$layers[[1]]$geom_params$raster

  mem <- pdr_plot_xy(pen_pd, method = "memory")
  file <- pdr_plot_xy(pen_pd, method = "file")

  expect_s3_class(file, "ggplot")
  expect_equal(layer_geoms(file), "GeomRasterAnn")
  expect_equal(file$coordinates$ratio, mem$coordinates$ratio)

  mem_r <- raster_of(mem)
  file_r <- raster_of(file)
  expect_equal(dim(file_r), dim(mem_r))

  # Same stroke thickness: share of drawn pixels agrees within 5% (relative)
  drawn <- \(r) mean(grDevices::col2rgb(r, alpha = TRUE)["alpha", ] > 0)
  expect_equal(drawn(file_r), drawn(mem_r), tolerance = 0.05)
})

test_that("dpi is no longer an argument", {
  expect_error(pdr_plot_xy(pen_pd, dpi = 300), "unused argument")
})

test_that("an invalid method errors", {
  expect_error(pdr_plot_xy(pen_pd, method = "nope"), "should be one of")
})

test_that("background image can be a path or a png array", {
  expect_s3_class(pdr_plot_xy(pen_pd, background_image = bg_png), "ggplot")
  expect_s3_class(
    pdr_plot_xy(pen_pd, background_image = png::readPNG(bg_png)),
    "ggplot"
  )

  vec <- pdr_plot_xy(pen_pd, background_image = bg_png, rasterize = FALSE)
  expect_equal(layer_geoms(vec)[1], "GeomRasterAnn")
})

test_that("invalid background images give informative errors", {
  expect_error(
    pdr_plot_xy(pen_pd, background_image = "does_not_exist.png"),
    "does not exist"
  )

  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_error(pdr_plot_xy(pen_pd, background_image = txt), "PNG file")

  expect_error(
    pdr_plot_xy(pen_pd, background_image = 1:3),
    "must be a PNG file path or a raster array"
  )
})

test_that("drawings with different canvas sizes are rejected", {
  mixed <- dplyr::bind_rows(pen_pd[1, ], spray_pd(1))

  expect_error(pdr_plot_xy(mixed), "same canvas dimensions")
})

test_that("missing drawings are dropped with a warning", {
  with_na <- dplyr::bind_rows(pen_pd, pen_pd[1, ])
  with_na$pdr_data[[3]] <- NA

  expect_warning(
    p <- pdr_plot_xy(with_na, rasterize = FALSE),
    "Dropping 1 missing drawing"
  )
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2)
})

test_that("only missing drawings gives an error", {
  all_na <- pen_pd
  all_na$pdr_data <- list(NA, NULL)

  expect_error(
    suppressWarnings(pdr_plot_xy(all_na)),
    "No non-missing drawings"
  )
})

test_that("more than 9 drawings warns about speed", {
  many <- pdr_example_data[1:10, ]

  expect_warning(
    pdr_plot_xy(many, rasterize = FALSE),
    "Recreating 10 drawings"
  )
})

test_that("spray radius is uniform, clustering dots towards the centre", {
  set.seed(9034)
  p <- pdr_plot_xy(spray_pd(2), rasterize = FALSE)
  d <- p$layers[[which(layer_geoms(p) == "GeomTile")]]$data

  # runif(0, 1) radius has mean 0.5 (sqrt-transformed would give 2/3)
  expect_equal(mean(d$radius / d$.spray_radius), 0.5, tolerance = 0.05)
  expect_true(all(d$radius <= d$.spray_radius))
})

test_that("spray dots are squares with side equal to the tool width", {
  p <- pdr_plot_xy(spray_pd(1), rasterize = FALSE)
  tiles <- ggplot2::layer_data(p, which(layer_geoms(p) == "GeomTile"))
  strokes <- spray_pd(1)$pdr_data[[1]]$.strokes

  expect_equal(
    tiles$xmax - tiles$xmin,
    rep(strokes$.tool_width[1], nrow(tiles))
  )
  expect_equal(
    tiles$ymax - tiles$ymin,
    rep(strokes$.tool_width[1], nrow(tiles))
  )
})

# Comparison with the REDCap PNG export, which contains background AND drawing.
# Spray jitter is random, so only footprint and ink are compared, with tolerance.
png_ink <- function(img, bg) {
  dif <- apply(abs(img[,, 1:3] - bg), c(1, 2), max)
  c(ink = sum(dif), px = sum(dif > 0.05))
}

raster_to_array <- function(r) {
  m <- grDevices::col2rgb(r) / 255
  array(
    c(
      matrix(m[1, ], nrow(r), ncol(r), byrow = TRUE),
      matrix(m[2, ], nrow(r), ncol(r), byrow = TRUE),
      matrix(m[3, ], nrow(r), ncol(r), byrow = TRUE)
    ),
    dim = c(nrow(r), ncol(r), 3)
  )
}

test_that("spray matches the REDCap PNG export in footprint and ink", {
  bg_file <- pdr_example_file("bg_lower_arm_palmar.png")
  bg <- png::readPNG(bg_file)

  for (i in 1:2) {
    ref <- png::readPNG(test_path(
      "fixtures",
      "png",
      paste0(i, "_palmar_png.png")
    ))
    set.seed(3100 + i)
    mine <- rowMeans(replicate(5, {
      p <- pdr_plot_xy(spray_pd(i), background_image = bg_file)
      png_ink(raster_to_array(p$layers[[1]]$geom_params$raster), bg)
    }))
    ref_stats <- png_ink(ref, bg)

    expect_equal(mine[["ink"]], ref_stats[["ink"]], tolerance = 0.15)
    expect_equal(mine[["px"]], ref_stats[["px"]], tolerance = 0.15)
  }
})

test_that("pen strokes are drawn .tool_width pixels wide", {
  line_width_px <- function(w, method = "memory") {
    x <- pen_pd[1, ]
    dr <- x$pdr_data[[1]]
    dr$.strokes <- dr$.strokes[1, ]
    dr$.strokes$.tool_width <- w
    dr$.strokes$.alpha <- 1
    dr$.strokes$.color <- "#000000"
    dr$.points <- tibble::tibble(
      .index = dr$.strokes$.index,
      .x = c(50, 400),
      .y = c(250, 250)
    )
    x$pdr_data[[1]] <- dr

    r <- pdr_plot_xy(x, method = method)$layers[[1]]$geom_params$raster
    alpha <- grDevices::col2rgb(r, alpha = TRUE)["alpha", ] / 255
    # Summed alpha down a column crossing the line = line thickness in pixels
    sum(matrix(alpha, nrow(r), ncol(r), byrow = TRUE)[, 225])
  }

  for (w in c(1, 5, 20)) {
    expect_equal(line_width_px(w), w, tolerance = 0.02)
  }
  expect_equal(line_width_px(5, method = "file"), 5, tolerance = 0.02)
})
