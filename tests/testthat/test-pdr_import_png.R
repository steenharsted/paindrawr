png_fixture <- function(n) {
  test_path("fixtures", "png", paste0(n, "_palmar_png.png"))
}

# Small generated fixtures; see fixtures/make_png_fixtures.R for the pixels.
# Expected values are taken from png::readPNG() on the same file, so 8-bit
# rounding does not matter.
tiny_png <- function(name) test_path("fixtures", "png", name)

test_that("a real RGBA PNG is imported as a pdr_tibble", {
  res <- pdr_import_png(png_fixture(1))

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("pdr_data", "id"))
  expect_equal(nrow(res), 1)
  expect_equal(res$id, "1_palmar_png")

  drawing <- res$pdr_data[[1]]
  expect_setequal(
    names(drawing),
    c(
      ".id",
      ".file",
      ".version",
      ".width",
      ".height",
      ".units",
      ".timestamp",
      ".app",
      ".points",
      ".strokes",
      ".rgba"
    )
  )
  expect_equal(drawing$.file, png_fixture(1))
  expect_equal(drawing$.units, "px")
  expect_equal(drawing$.width, 624)
  expect_equal(drawing$.height, 720)
  expect_equal(dim(drawing$.rgba), c(720, 624, 4))
})

test_that("multiple files give one row each, in input order", {
  res <- pdr_import_png(c(png_fixture(2), png_fixture(1)))

  expect_equal(nrow(res), 2)
  expect_equal(res$id, c("2_palmar_png", "1_palmar_png"))
})

test_that("`.points` and `.strokes` are empty tibbles", {
  drawing <- pdr_import_png(png_fixture(1))$pdr_data[[1]]

  expect_s3_class(drawing$.points, "tbl_df")
  expect_equal(nrow(drawing$.points), 0)
  expect_named(drawing$.points, c(".index", ".x", ".y"))
  expect_s3_class(drawing$.strokes, "tbl_df")
  expect_equal(nrow(drawing$.strokes), 0)
  expect_true(".index" %in% names(drawing$.strokes))
})

test_that("`id_from_filename` controls `.id`", {
  path <- tiny_png("rgb_3ch.png")

  expect_equal(
    pdr_import_png(path)$id,
    tools::file_path_sans_ext(basename(path))
  )
  res <- pdr_import_png(path, id_from_filename = FALSE)
  expect_identical(res$id, NA_character_)
  expect_identical(res$pdr_data[[1]]$.id, NA_character_)
})

test_that("grayscale (1 channel) is replicated to RGB with alpha 1", {
  path <- tiny_png("gray_1ch.png")
  expected <- png::readPNG(path)

  rgba <- pdr_import_png(path)$pdr_data[[1]]$.rgba
  expect_equal(dim(rgba), c(2, 3, 4))
  expect_equal(rgba[,, 1], expected)
  expect_equal(rgba[,, 2], expected)
  expect_equal(rgba[,, 3], expected)
  expect_true(all(rgba[,, 4] == 1))
})

test_that("grayscale + alpha (2 channels) keeps alpha", {
  path <- tiny_png("gray_alpha_2ch.png")
  expected <- png::readPNG(path)

  rgba <- pdr_import_png(path)$pdr_data[[1]]$.rgba
  expect_equal(dim(rgba), c(2, 2, 4))
  expect_equal(rgba[,, 4], expected[,, 2])
  # Fully transparent pixel (row 2, col 1) is set to black
  expect_equal(rgba[2, 1, 1:3], c(0, 0, 0))
  # Opaque pixel keeps its gray value in R, G and B
  expect_equal(rgba[1, 1, 1:3], rep(expected[1, 1, 1], 3))
})

test_that("RGB (3 channels) gets alpha 1 and unchanged colours", {
  path <- tiny_png("rgb_3ch.png")
  expected <- png::readPNG(path)

  rgba <- pdr_import_png(path)$pdr_data[[1]]$.rgba
  expect_equal(dim(rgba), c(2, 3, 4))
  expect_equal(rgba[,, 1:3], expected)
  expect_true(all(rgba[,, 4] == 1))
})

test_that("RGBA (4 channels) is kept, with transparent pixels set to black", {
  path <- tiny_png("rgba_transparent_4ch.png")
  expected <- png::readPNG(path)

  rgba <- pdr_import_png(path)$pdr_data[[1]]$.rgba
  expect_equal(rgba[1, 1, ], c(0, 0, 0, 0))
  expect_equal(rgba[2, 2, ], expected[2, 2, ])
  expect_equal(rgba[1, 2, ], expected[1, 2, ])
})

test_that("`background` makes matching pixels transparent (alpha 0, black)", {
  path <- tiny_png("rgb_3ch.png")

  rgba <- pdr_import_png(path, background = "white")$pdr_data[[1]]$.rgba
  is_white <- rgba[,, 4] == 0
  # rgb_3ch.png has two white pixels (column 2)
  expect_equal(sum(is_white), 2)
  expect_true(all(is_white[, 2]))
  expect_true(all(rgba[,, 1:3][rep(is_white, 3)] == 0))
  expect_true(all(rgba[,, 4][!is_white] == 1))
})

test_that("`background` accepts hex colours and black", {
  path <- tiny_png("rgb_3ch.png")

  rgba <- pdr_import_png(path, background = "#000000")$pdr_data[[1]]$.rgba
  # rgb_3ch.png has a single black pixel (row 1, column 3)
  expect_equal(which(rgba[,, 4] == 0), 5L)
})

test_that("`background_tolerance` widens the background match", {
  # Near-white pixel (.9) next to white (1) and black (0)
  path <- tiny_png("gray_near_white_1ch.png")

  exact <- pdr_import_png(path, background = "white")$pdr_data[[1]]$.rgba
  expect_equal(sum(exact[,, 4] == 0), 2)

  tolerant <- pdr_import_png(
    path,
    background = "white",
    background_tolerance = 0.15
  )$pdr_data[[1]]$.rgba
  expect_equal(sum(tolerant[,, 4] == 0), 3)
})

test_that("`background` is ignored for PNGs with an alpha channel", {
  path <- tiny_png("rgba_opaque_white_4ch.png")

  rgba <- pdr_import_png(path, background = "white")$pdr_data[[1]]$.rgba
  expect_true(all(rgba[,, 4] == 1))
})

test_that("`files` must be a non-empty character vector", {
  expect_error(pdr_import_png(character()), "`files` must be")
  expect_error(pdr_import_png(NULL), "`files` must be")
  expect_error(pdr_import_png(1), "`files` must be")
  expect_error(pdr_import_png(list(png_fixture(1))), "`files` must be")
})

test_that("missing files give a warning naming the files", {
  expect_warning(
    pdr_import_png(c(png_fixture(1), "nope_a.png", "nope_b.png")),
    "nope_a.png, nope_b.png",
    fixed = TRUE
  )
})

test_that("NA entries give NA out, without error", {
  res <- pdr_import_png(c(png_fixture(1), NA))
  expect_equal(nrow(res), 2)
  expect_identical(res$pdr_data[[2]], NA)
  expect_identical(res$id[[2]], NA_character_)
  expect_equal(dim(res$pdr_data[[1]]$.rgba)[3], 4)
})

test_that("`background_tolerance` is validated", {
  path <- tiny_png("rgb_3ch.png")

  expect_error(
    pdr_import_png(path, background_tolerance = -0.1),
    "between 0 and 1"
  )
  expect_error(
    pdr_import_png(path, background_tolerance = 1.1),
    "between 0 and 1"
  )
  expect_error(
    pdr_import_png(path, background_tolerance = "a"),
    "between 0 and 1"
  )
  expect_error(
    pdr_import_png(path, background_tolerance = c(0, 0.1)),
    "between 0 and 1"
  )
})

test_that("`background` is validated", {
  path <- tiny_png("rgb_3ch.png")

  expect_error(
    pdr_import_png(path, background = c("white", "black")),
    "single colour"
  )
  expect_error(pdr_import_png(path, background = "not_a_colour"))
})

test_that("non-existing files warn and give NA with a file-derived id", {
  expect_warning(
    res <- pdr_import_png(c(png_fixture(1), "does_not_exist.png", NA)),
    "Non-existing PNG files"
  )
  expect_equal(nrow(res), 3)
  expect_identical(res$pdr_data[[2]], NA)
  expect_identical(res$pdr_data[[3]], NA)
  expect_identical(res$id, c("1_palmar_png", "does_not_exist", NA_character_))

  res <- suppressWarnings(
    pdr_import_png("does_not_exist.png", id_from_filename = FALSE)
  )
  expect_identical(res$id, NA_character_)
})

test_that("to_rgba() rejects unsupported channel layouts", {
  expect_error(
    to_rgba(array(0, dim = c(2, 2, 5)), "x.png"),
    "Unsupported PNG channel layout"
  )
})
