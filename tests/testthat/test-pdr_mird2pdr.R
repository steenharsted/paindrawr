two_strokes <- "(,1,2,5,4,),(,8,8,7,7,)"

test_that("MIRD strings are converted to points with a stroke index", {
  res <- pdr_mird2pdr(two_strokes, id = "a", flipy = FALSE)

  expect_type(res, "list")
  expect_length(res, 1)
  points <- res[[1]]$.points
  expect_s3_class(points, "tbl_df")
  expect_setequal(names(points), c(".index", ".x", ".y"))
  expect_equal(points$.x, c(1, 5, 8, 7))
  expect_equal(points$.y, c(2, 4, 8, 7))
  expect_equal(points$.index, c(1, 1, 2, 2))
})

test_that("a longer single stroke is parsed in order", {
  res <- pdr_mird2pdr(
    "(,460,709,463,709,464,719,448,720,449,712,)",
    id = "a",
    flipy = FALSE
  )
  points <- res[[1]]$.points

  expect_equal(points$.x, c(460, 463, 464, 448, 449))
  expect_equal(points$.y, c(709, 709, 719, 720, 712))
  expect_true(all(points$.index == 1))
})

test_that("three strokes get indices 1, 2 and 3", {
  res <- pdr_mird2pdr("(,1,1,),(,2,2,3,3,),(,4,4,)", id = "a", flipy = FALSE)

  expect_equal(res[[1]]$.points$.index, c(1, 2, 2, 3))
  expect_equal(res[[1]]$.strokes$.index, 1:3)
})

test_that("`flipy` flips y using the canvas height", {
  flipped <- pdr_mird2pdr(two_strokes, id = "a", height = 100L)
  unflipped <- pdr_mird2pdr(two_strokes, id = "a", height = 100L, flipy = FALSE)

  expect_equal(flipped[[1]]$.points$.y, 100 - unflipped[[1]]$.points$.y)
  expect_equal(flipped[[1]]$.points$.x, unflipped[[1]]$.points$.x)
})

test_that("`flipy` is on by default and uses each drawing's own height", {
  res <- pdr_mird2pdr(
    c("(,1,10,)", "(,1,10,)"),
    id = c("a", "b"),
    height = c(100L, 200L)
  )

  expect_equal(res[[1]]$.points$.y, 90)
  expect_equal(res[[2]]$.points$.y, 190)
})

test_that("each drawing has the documented elements and defaults", {
  drawing <- pdr_mird2pdr(two_strokes, id = "a")[[1]]

  expect_named(
    drawing,
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
      ".strokes"
    )
  )
  expect_equal(drawing$.id, "a")
  expect_equal(drawing$.file, "")
  expect_equal(drawing$.version, "")
  expect_equal(drawing$.width, 450)
  expect_equal(drawing$.height, 500)
  expect_equal(drawing$.units, "px")
  expect_equal(drawing$.timestamp, "")
  expect_equal(drawing$.app, "")
})

test_that("metadata parameters are stored in the drawing", {
  drawing <- pdr_mird2pdr(
    two_strokes,
    id = "a",
    file = "f.csv",
    version = "v1",
    width = 300L,
    height = 400L,
    units = "mm",
    timestamp = "2020-01-01",
    app = "SurveyXact"
  )[[1]]

  expect_equal(drawing$.file, "f.csv")
  expect_equal(drawing$.version, "v1")
  expect_equal(drawing$.width, 300)
  expect_equal(drawing$.height, 400)
  expect_equal(drawing$.units, "mm")
  expect_equal(drawing$.timestamp, "2020-01-01")
  expect_equal(drawing$.app, "SurveyXact")
})

test_that(".strokes has one row per stroke with the stroke parameters", {
  strokes <- pdr_mird2pdr(
    two_strokes,
    id = "a",
    tool = "spray",
    draw_input_type = "touch",
    tool_width = 7L,
    point_density = 20L,
    spray_radius = 15L,
    color = "#00ff00"
  )[[1]]$.strokes

  expect_equal(nrow(strokes), 2)
  expect_setequal(
    names(strokes),
    c(
      ".index",
      ".tool",
      ".draw_input_type",
      ".tool_width",
      ".point_density",
      ".spray_radius",
      ".alpha",
      ".color"
    )
  )
  expect_equal(strokes$.index, 1:2)
  expect_equal(strokes$.tool, c("spray", "spray"))
  expect_equal(strokes$.draw_input_type, c("touch", "touch"))
  expect_equal(strokes$.tool_width, c(7, 7))
  expect_equal(strokes$.point_density, c(20, 20))
  expect_equal(strokes$.spray_radius, c(15, 15))
  expect_equal(strokes$.color, c("#00ff00", "#00ff00"))
})

test_that("`id` defaults to the position in `mird_pd`", {
  res <- pdr_mird2pdr(c("(,1,2,)", "(,3,4,)", "(,5,6,)"))

  expect_equal(vapply(res, \(x) x$.id, integer(1)), 1:3)
})

test_that("zero-length input returns an empty list", {
  expect_identical(pdr_mird2pdr(character()), list())
})

test_that("empty and too short strings give drawings without points", {
  mird <- c("(,1,2,)", "", "(,1,)")
  res <- pdr_mird2pdr(mird, id = letters[1:3], flipy = FALSE)

  expect_length(res, 3)
  expect_equal(nrow(res[[1]]$.points), 1)
  for (i in 2:3) {
    expect_equal(nrow(res[[i]]$.points), 0, info = paste("element", i))
    expect_equal(nrow(res[[i]]$.strokes), 0, info = paste("element", i))
    expect_setequal(names(res[[i]]$.points), c(".index", ".x", ".y"))
    # Metadata is kept
    expect_equal(res[[i]]$.id, letters[i])
    expect_equal(res[[i]]$.width, 450)
  }
})

test_that

test_that("parameters of length n are applied per drawing", {
  res <- pdr_mird2pdr(
    c("(,1,2,)", "(,3,4,)"),
    id = c("a", "b"),
    width = c(100L, 200L),
    tool = c("pen", "spray"),
    color = c("#111111", "#222222"),
    flipy = FALSE
  )

  expect_equal(vapply(res, \(x) x$.width, integer(1)), c(100L, 200L))
  expect_equal(res[[1]]$.strokes$.tool, "pen")
  expect_equal(res[[2]]$.strokes$.tool, "spray")
  expect_equal(res[[1]]$.strokes$.color, "#111111")
  expect_equal(res[[2]]$.strokes$.color, "#222222")
})

test_that("parameters of other lengths than 1 or n give an error", {
  mird <- c("(,1,2,)", "(,3,4,)", "(,5,6,)")

  expect_error(
    pdr_mird2pdr(mird, id = c("a", "b")),
    "`id` has length 2"
  )
  # All offending parameters are named
  expect_error(
    pdr_mird2pdr(mird, id = c("a", "b"), tool = c("pen", "spray")),
    "`tool` has length 2"
  )
})

test_that("a non-character `mird_pd` gives a warning", {
  expect_warning(pdr_mird2pdr(123, id = "a"), "wrong type")
})

test_that("the result works as a pdr_list column in a tibble", {
  res <- tibble::tibble(
    id = c("a", "b", "c"),
    mird = c("(,1,2,)", NA, two_strokes)
  ) |>
    dplyr::mutate(pdr_data = pdr_mird2pdr(mird, id = id))

  expect_equal(nrow(res), 3)
  expect_equal(
    vapply(res$pdr_data, \(x) x$.id, character(1)),
    c("a", "b", "c")
  )
})

test_that("`alpha` defaults to 1 and a given value is stored as is", {
  default <- pdr_mird2pdr(two_strokes, id = "a")[[1]]$.strokes
  expect_equal(default$.alpha, c(1, 1))
  expect_type(default$.alpha, "double")

  given <- pdr_mird2pdr(two_strokes, id = "a", alpha = 0.5)[[1]]$.strokes
  expect_equal(given$.alpha, c(0.5, 0.5))
})

test_that("`alpha` must be numeric between 0 and 1", {
  expect_error(
    pdr_mird2pdr(two_strokes, id = "a", alpha = 1.5),
    "between 0 and 1"
  )
  expect_error(
    pdr_mird2pdr(two_strokes, id = "a", alpha = -0.1),
    "between 0 and 1"
  )
  expect_error(
    pdr_mird2pdr(two_strokes, id = "a", alpha = NA_real_),
    "between 0 and 1"
  )
  expect_error(
    pdr_mird2pdr(two_strokes, id = "a", alpha = "0.5"),
    "between 0 and 1"
  )
  expect_no_error(pdr_mird2pdr(two_strokes, id = "a", alpha = c(0, 1)[1]))
})

test_that("empty and non-empty .strokes have the same column types", {
  filled <- pdr_mird2pdr(two_strokes, id = "a")[[1]]$.strokes
  empty <- pdr_mird2pdr(NA_character_, id = "a")[[1]]$.strokes

  expect_identical(
    vapply(empty, \(x) class(x)[1], character(1)),
    vapply(filled, \(x) class(x)[1], character(1))
  )
  expect_type(empty$.index, "integer")
})

test_that("an odd number of coordinates in a stroke is an error", {
  expect_error(
    pdr_mird2pdr("(,1,2,3,)", id = "a"),
    "odd number of coordinates"
  )
  # Only the second stroke is corrupt; the message points to it
  expect_error(
    pdr_mird2pdr("(,1,2,),(,3,4,5,)", id = "a"),
    "Stroke 2 has 3 values"
  )
})
