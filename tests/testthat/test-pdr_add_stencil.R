# Synthetic 2 x 3 px drawing with known channel values
make_drawing <- function(r = 0, g = 0, b = 0, a = 0) {
  rgba <- array(0, dim = c(2, 3, 4))
  rgba[,, 1] <- r
  rgba[,, 2] <- g
  rgba[,, 3] <- b
  rgba[,, 4] <- a
  list(.id = "1", .width = 3, .height = 2, .rgba = rgba)
}

# Alpha: 0, 0.4, 0.5 / 0.6, 1, 1 (row 1 / row 2)
alpha_mat <- matrix(c(0, 0.4, 0.5, 0.6, 1, 1), nrow = 2, byrow = TRUE)
drawing <- make_drawing(a = alpha_mat)

stencil_of <- function(..., d = drawing) {
  pdr_add_stencil(list(d), ...)[[1]]$.stencil
}

example_rgba <- suppressMessages(
  pdr_example_data[1:3, ] |>
    dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
)

test_that("adds .stencil and .stencil_inverted, keeping other elements", {
  res <- pdr_add_stencil(list(drawing))

  expect_length(res, 1)
  expect_true(all(
    c(".stencil", ".stencil_inverted", ".rgba", ".id") %in% names(res[[1]])
  ))
  expect_equal(res[[1]][names(drawing)], drawing)
})

test_that("stencil is a 0/1 matrix with the drawing's height x width", {
  s <- stencil_of()

  expect_true(is.matrix(s))
  expect_equal(dim(s), c(2, 3))
  expect_true(all(s %in% c(0, 1)))
  expect_true(is.numeric(s))
})

test_that("inverted stencil swaps 0 and 1", {
  res <- pdr_add_stencil(list(drawing), threshold = 0.5)[[1]]

  expect_equal(res$.stencil_inverted, 1 - res$.stencil)
  expect_true(all(res$.stencil + res$.stencil_inverted == 1))
})

test_that("defaults mark pixels that are not fully opaque", {
  # threshold = 1, by = "<", channel = "A"
  expect_equal(stencil_of(), (alpha_mat < 1) + 0)
  expect_equal(stencil_of()[2, 2:3], c(0, 0))
})

test_that("by = '<' and '>' use a strict comparison with the threshold", {
  expect_equal(stencil_of(threshold = 0.5, by = "<"), (alpha_mat < 0.5) + 0)
  expect_equal(stencil_of(threshold = 0.5, by = ">"), (alpha_mat > 0.5) + 0)

  # A pixel exactly at the threshold is 0 for both
  expect_equal(stencil_of(threshold = 0.5, by = "<")[1, 3], 0)
  expect_equal(stencil_of(threshold = 0.5, by = ">")[1, 3], 0)
})

test_that("channel selects which channels are evaluated", {
  d <- make_drawing(
    r = matrix(c(1, 0, 1, 1, 1, 1), 2, byrow = TRUE),
    g = 1,
    b = 1,
    a = 1
  )

  # Only red is below the threshold at (1, 2)
  expect_equal(
    stencil_of(channel = "R", d = d),
    matrix(c(0, 1, 0, 0, 0, 0), 2, byrow = TRUE)
  )
  # Other channels are all 1, so nothing is below the threshold
  expect_true(all(stencil_of(channel = "G", d = d) == 0))
  expect_true(all(stencil_of(channel = "B", d = d) == 0))
  expect_true(all(stencil_of(channel = "A", d = d) == 0))
})

test_that("several channels are combined with OR", {
  d <- make_drawing(
    r = matrix(c(0, 1, 1, 1, 1, 1), 2, byrow = TRUE),
    g = matrix(c(1, 0, 1, 1, 1, 1), 2, byrow = TRUE),
    b = matrix(c(1, 1, 0, 1, 1, 1), 2, byrow = TRUE),
    a = 1
  )

  expect_equal(
    stencil_of(channel = "RGB", d = d),
    matrix(c(1, 1, 1, 0, 0, 0), 2, byrow = TRUE)
  )
  expect_equal(
    stencil_of(channel = "RG", d = d),
    matrix(c(1, 1, 0, 0, 0, 0), 2, byrow = TRUE)
  )
  # Order of letters does not matter
  expect_equal(
    stencil_of(channel = "GR", d = d),
    stencil_of(channel = "RG", d = d)
  )
  expect_equal(
    stencil_of(channel = "RGBA", d = d),
    stencil_of(channel = "RGB", d = d)
  )
})

test_that("a list of drawings returns a list of the same length", {
  res <- pdr_add_stencil(list(drawing, drawing, drawing))

  expect_length(res, 3)
  expect_true(all(vapply(res, \(x) ".stencil" %in% names(x), logical(1))))
})

test_that("each drawing gets its own stencil", {
  other <- make_drawing(a = 1)
  res <- pdr_add_stencil(list(drawing, other))

  expect_false(identical(res[[1]]$.stencil, res[[2]]$.stencil))
  expect_true(all(res[[2]]$.stencil == 0))
})

test_that("works on a pdr_list column from rendered example data", {
  res <- pdr_add_stencil(example_rgba$pdr_data, threshold = 0.5)

  expect_length(res, 3)
  expect_equal(dim(res[[1]]$.stencil), c(500, 450))
  expect_true(all(res[[1]]$.stencil %in% c(0, 1)))
  # Drawn pixels are opaque (stencil 0), undrawn are transparent (stencil 1)
  drawn <- res[[1]]$.rgba[,, 4] >= 0.5
  expect_true(any(drawn))
  expect_true(all(res[[1]]$.stencil[drawn] == 0))
  expect_true(all(res[[1]]$.stencil[!drawn] == 1))
})

test_that("works inside mutate on a pdr_tibble", {
  res <- example_rgba |>
    dplyr::mutate(pdr_data = pdr_add_stencil(pdr_data))

  expect_equal(nrow(res), 3)
  expect_true(all(vapply(
    res$pdr_data,
    \(x) all(c(".stencil", ".stencil_inverted") %in% names(x)),
    logical(1)
  )))
})

test_that("input without .rgba errors", {
  expect_error(
    pdr_add_stencil(pdr_example_data$pdr_data[1:2]),
    "\\.rgba"
  )
})

test_that("invalid by errors", {
  expect_error(pdr_add_stencil(list(drawing), by = "="), "'by' is invalid")
  expect_error(pdr_add_stencil(list(drawing), by = "nope"), "'by' is invalid")
})

test_that("invalid channel errors", {
  expect_error(
    pdr_add_stencil(list(drawing), channel = "X"),
    "Invalid parameter 'channel'"
  )
  expect_error(
    pdr_add_stencil(list(drawing), channel = ""),
    "Invalid parameter 'channel'"
  )
})

test_that("input that is not a pdr_list errors", {
  expect_error(pdr_add_stencil("not a list"))
  expect_error(pdr_add_stencil(1:3))
})
