# Synthetic 2 x 3 px drawings with known stencils
make_stencil_drawing <- function(
  id,
  stencil,
  file = "f.json",
  ts = "2026-01-01"
) {
  list(
    .id = id,
    .file = file,
    .version = 1L,
    .width = 3L,
    .height = 2L,
    .units = "px",
    .timestamp = ts,
    .app = "app",
    .points = tibble::tibble(
      .index = integer(),
      .x = integer(),
      .y = integer()
    ),
    .strokes = tibble::tibble(.index = integer()),
    .stencil = stencil,
    .stencil_inverted = 1 - stencil
  )
}

a <- make_stencil_drawing("a", matrix(c(1, 0, 0, 0, 0, 0), nrow = 2))
b <- make_stencil_drawing("b", matrix(c(0, 0, 1, 0, 0, 1), nrow = 2))

test_that("returns a length-1 pdr_list holding a single pdr_drawing", {
  res <- pdr_implode_stencils(list(a, b))

  expect_length(res, 1)
  expect_type(res, "list")
  expect_equal(res[[1]]$.width, 3L)
  expect_equal(res[[1]]$.height, 2L)
  expect_equal(res[[1]]$.units, "px")
})

test_that("collapsed stencil is the union of the input 1-regions", {
  res <- pdr_implode_stencils(list(a, b))

  expect_equal(res[[1]]$.stencil, matrix(c(1, 0, 1, 0, 0, 1), nrow = 2))
  expect_equal(res[[1]]$.stencil_inverted, 1 - res[[1]]$.stencil)
  expect_true(all(res[[1]]$.stencil %in% c(0, 1)))
})

test_that("overlapping stencils still give a 0/1 union", {
  both <- list(
    a,
    make_stencil_drawing("a2", matrix(c(1, 1, 0, 0, 0, 0), nrow = 2))
  )
  res <- pdr_implode_stencils(both)

  expect_equal(res[[1]]$.stencil, matrix(c(1, 1, 0, 0, 0, 0), nrow = 2))
})

test_that("id comes from the id argument or defaults to collapsed_<time>", {
  res <- pdr_implode_stencils(list(a, b), id = "my_region")

  expect_equal(res[[1]]$.id, "my_region")

  res_default <- pdr_implode_stencils(list(a, b))
  expect_match(res_default[[1]]$.id, "^collapsed_")
  expect_type(res_default[[1]]$.id, "character")
})

test_that("metadata common to all inputs is kept, otherwise replaced", {
  other_file <- make_stencil_drawing("b", b$.stencil, file = "g.json")
  other_ts <- make_stencil_drawing("b", b$.stencil, ts = "2026-02-02")

  res <- pdr_implode_stencils(list(a, b))
  expect_equal(res[[1]]$.file, "f.json")
  expect_equal(res[[1]]$.version, 1L)
  expect_equal(res[[1]]$.app, "app")
  expect_equal(res[[1]]$.timestamp, "2026-01-01")

  expect_equal(pdr_implode_stencils(list(a, other_file))[[1]]$.file, "multiple")
  expect_equal(
    pdr_implode_stencils(list(a, other_ts))[[1]]$.timestamp,
    "2026-02-02"
  )
})

test_that("timestamp is NA when all input timestamps are NA", {
  na_ts <- function(x) {
    x$.timestamp <- NA_character_
    x
  }
  res <- pdr_implode_stencils(list(na_ts(a), na_ts(b)))

  expect_equal(res[[1]]$.timestamp, NA_character_)
})

test_that("result has empty points and strokes", {
  res <- pdr_implode_stencils(list(a, b))

  expect_equal(nrow(res[[1]]$.points), 0)
  expect_equal(nrow(res[[1]]$.strokes), 0)
  expect_equal(names(res[[1]]$.points), c(".index", ".x", ".y"))
  expect_equal(names(res[[1]]$.strokes), ".index")
})

test_that("rgba alpha channel holds the collapsed stencil", {
  res <- pdr_implode_stencils(list(a, b))
  rgba <- res[[1]]$.rgba

  expect_equal(dim(rgba), c(2, 3, 4))
  expect_equal(rgba[,, 4], res[[1]]$.stencil)
  # RGB channels are all 0 (black)
  expect_true(all(rgba[,, 1:3] == 0))
})

test_that("a single pdr_drawing input is coerced to a pdr_list", {
  res <- pdr_implode_stencils(a)

  expect_length(res, 1)
  expect_equal(res[[1]]$.stencil, a$.stencil)
})

test_that("drawings missing .stencil error", {
  no_stencil <- a
  no_stencil$.stencil <- NULL
  no_stencil$.stencil_inverted <- NULL

  expect_snapshot(
    pdr_implode_stencils(list(no_stencil, b)),
    error = TRUE
  )
})

test_that("drawings with different canvas dimensions error", {
  wide <- a
  wide$.width <- 4L
  wide$.stencil <- matrix(0, nrow = 2, ncol = 4)

  expect_snapshot(pdr_implode_stencils(list(a, wide)), error = TRUE)
})

test_that("stencil matrices with wrong dimensions error", {
  wrong <- a
  wrong$.stencil <- matrix(0, nrow = 3, ncol = 2)

  expect_snapshot(pdr_implode_stencils(list(a, wrong)), error = TRUE)
})

test_that("drawings with different units error", {
  inches <- b
  inches$.units <- "in"

  expect_snapshot(pdr_implode_stencils(list(a, inches)), error = TRUE)
})
