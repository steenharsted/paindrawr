json_fixture <- function(n) {
  test_path("fixtures", "json", paste0(n, "_palmar.json"))
}

# JSON fixtures contain spray strokes (624 x 720); example data contains pen
# strokes (450 x 500).
spray_drawing <- function(n = 1) pdr_import_json(json_fixture(n))$pdr_data[[1]]
pen_drawing <- pdr_example_data$pdr_data[[1]]

alpha_of <- function(rgba) rgba[,, 4]

test_that("a single drawing gets a height x width x 4 .rgba array", {
  res <- pdr_add_rgba(pen_drawing)

  expect_true(".rgba" %in% names(res))
  expect_equal(dim(res$.rgba), c(500, 450, 4))
  expect_true(all(res$.rgba >= 0 & res$.rgba <= 1))
  expect_equal(res[names(res) != ".rgba"], pen_drawing)
})

test_that("rgba_only returns only the array", {
  res <- pdr_add_rgba(pen_drawing, rgba_only = TRUE)

  expect_named(res, ".rgba")
  expect_equal(dim(res$.rgba), c(500, 450, 4))
})

test_that("a list of drawings returns a list of the same length", {
  res <- pdr_add_rgba(pdr_example_data$pdr_data[1:3])

  expect_length(res, 3)
  expect_true(all(vapply(res, \(x) ".rgba" %in% names(x), logical(1))))

  # A list of length 1 stays a list
  expect_length(pdr_add_rgba(list(pen_drawing)), 1)
})

test_that("NULL returns NULL", {
  expect_null(pdr_add_rgba(NULL))
})

test_that("invalid method and polygon arguments error", {
  expect_error(pdr_add_rgba(pen_drawing, method = "nope"), "should be one of")
  expect_error(pdr_add_rgba(pen_drawing, polygon = "nope"), "should be one of")
})

test_that("an empty drawing gives a fully transparent array", {
  empty <- pdr_import_json(json_fixture("empty"))$pdr_data[[1]]
  res <- pdr_add_rgba(empty, rgba_only = TRUE)

  expect_equal(dim(res$.rgba), c(720, 624, 4))
  expect_true(all(res$.rgba == 0))
})

test_that("transparent pixels are black", {
  rgba <- pdr_add_rgba(pen_drawing, rgba_only = TRUE)$.rgba
  transparent <- alpha_of(rgba) == 0

  expect_true(any(transparent))
  expect_true(all(rgba[,, 1:3][transparent] == 0))
})

test_that("pen strokes are drawn .tool_width pixels wide", {
  line_width_px <- function(w, method = "memory") {
    dr <- pen_drawing
    dr$.strokes <- dr$.strokes[1, ]
    dr$.strokes$.tool_width <- w
    dr$.strokes$.alpha <- 1
    dr$.strokes$.color <- "#000000"
    dr$.points <- tibble::tibble(
      .index = dr$.strokes$.index,
      .x = c(50, 400),
      .y = c(250, 250)
    )

    # Summed alpha down a column crossing the line = line thickness in pixels
    sum(
      alpha_of(pdr_add_rgba(dr, method = method, rgba_only = TRUE)$.rgba)[, 225]
    )
  }

  for (w in c(1, 5, 20)) {
    expect_equal(line_width_px(w), w, tolerance = 0.02)
  }
  expect_equal(line_width_px(5, method = "file"), 5, tolerance = 0.02)
})

test_that("spray dots are .tool_width squares around a uniform-radius point", {
  # One spray point, many dots; all dots lie within spray_radius + half a dot
  dr <- spray_drawing(1)
  dr$.strokes <- dr$.strokes[1, ]
  dr$.strokes$.point_density <- 2000L
  dr$.strokes$.alpha <- 1
  dr$.strokes$.color <- "#000000"
  w <- dr$.strokes$.tool_width
  r <- dr$.strokes$.spray_radius
  dr$.points <- tibble::tibble(.index = dr$.strokes$.index, .x = 300, .y = 360)

  set.seed(2718)
  a <- alpha_of(pdr_add_rgba(dr, rgba_only = TRUE)$.rgba)
  drawn <- which(a > 0, arr.ind = TRUE)

  # Extent of drawn pixels = spray radius + half a dot (canvas row = H - y)
  expect_lte(max(abs(drawn[, 2] - 300)), r + w / 2 + 1)
  expect_lte(max(abs(drawn[, 1] - (720 - 360))), r + w / 2 + 1)
  expect_gte(max(abs(drawn[, 2] - 300)), r - 2)
})

test_that("method = 'file' gives the same image as 'memory'", {
  mem <- alpha_of(pdr_add_rgba(pen_drawing, rgba_only = TRUE)$.rgba)
  file <- alpha_of(
    pdr_add_rgba(pen_drawing, method = "file", rgba_only = TRUE)$.rgba
  )

  expect_equal(dim(file), dim(mem))
  expect_equal(mean(file > 0), mean(mem > 0), tolerance = 0.05)
})

test_that("polygon argument works for pen and spray", {
  expect_equal(
    dim(pdr_add_rgba(pen_drawing, polygon = "pen", rgba_only = TRUE)$.rgba),
    c(500, 450, 4)
  )
  expect_equal(
    dim(
      pdr_add_rgba(spray_drawing(1), polygon = "both", rgba_only = TRUE)$.rgba
    ),
    c(720, 624, 4)
  )
})

# Comparison with the REDCap PNG export, which contains background AND drawing.
# Spray jitter is random, so only footprint and ink are compared, with tolerance.
test_that("spray matches the REDCap PNG export in footprint and ink", {
  bg <- png::readPNG(pdr_example_file("bg_lower_arm_palmar.png"))

  png_ink <- function(img) {
    dif <- apply(abs(img[,, 1:3] - bg), c(1, 2), max)
    c(ink = sum(dif), px = sum(dif > 0.05))
  }

  # Composite the .rgba drawing over the background (source-over)
  composite <- function(rgba) {
    a <- rgba[,, 4]
    out <- bg
    for (ch in 1:3) {
      out[,, ch] <- bg[,, ch] * (1 - a) + rgba[,, ch] * a
    }
    out
  }

  for (i in 1:2) {
    ref <- png::readPNG(test_path(
      "fixtures",
      "png",
      paste0(i, "_palmar_png.png")
    ))
    set.seed(5200 + i)
    mine <- rowMeans(replicate(5, {
      rgba <- pdr_add_rgba(spray_drawing(i), rgba_only = TRUE)$.rgba
      png_ink(composite(rgba))
    }))
    ref_stats <- png_ink(ref)

    expect_equal(mine[["ink"]], ref_stats[["ink"]], tolerance = 0.15)
    expect_equal(mine[["px"]], ref_stats[["px"]], tolerance = 0.15)
  }
})
