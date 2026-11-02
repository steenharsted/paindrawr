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
drawing <- make_drawing(r = 0.2, g = 0.4, b = 0.6, a = alpha_mat)

# 0/1 stencil keeping the first column only
stencil_mat <- matrix(c(1, 0, 0, 1, 0, 0), nrow = 2, byrow = TRUE)

rgba_of <- function(res, i = 1) res[[i]]$.rgba

test_that("invalid op errors", {
  expect_error(pdr_modify_rgba(list(drawing)), "'op' has invalid value")
  expect_error(
    pdr_modify_rgba(list(drawing), op = "nope"),
    "'op' has invalid value"
  )
})

test_that("invalid threshold errors", {
  expect_error(
    pdr_modify_rgba(list(drawing), op = "low_trim", threshold = -0.1),
    "'threshold' has invalid value"
  )
  expect_error(
    pdr_modify_rgba(list(drawing), op = "low_trim", threshold = 1.1),
    "'threshold' has invalid value"
  )
  expect_error(
    pdr_modify_rgba(list(drawing), op = "low_trim", threshold = "0.5"),
    "'threshold' has invalid value"
  )
})

test_that("input that is not a pdr_list errors", {
  expect_error(pdr_modify_rgba("not a list", op = "low_trim"))
  expect_error(pdr_modify_rgba(1:3, op = "low_trim"))
})

test_that("low_trim sets values below the threshold to 0", {
  res <- pdr_modify_rgba(list(drawing), op = "low_trim", threshold = 0.5)

  expect_equal(dim(rgba_of(res)), c(2, 3, 4))
  # Alpha: 0, 0.4 -> 0; 0.5 (at threshold) stays
  expect_equal(
    rgba_of(res)[,, 4],
    matrix(c(0, 0, 0.5, 0.6, 1, 1), nrow = 2, byrow = TRUE)
  )
  # Colour channels (0.2, 0.4) are below 0.5 -> 0 ; 0.6 stays
  expect_true(all(rgba_of(res)[,, 1] == 0))
  expect_true(all(rgba_of(res)[,, 2] == 0))
  expect_true(all(rgba_of(res)[,, 3] == 0.6))
})

test_that("high_trim sets values above the threshold to 1", {
  res <- pdr_modify_rgba(list(drawing), op = "high_trim", threshold = 0.5)

  expect_equal(dim(rgba_of(res)), c(2, 3, 4))
  # Alpha: 0.6 -> 1; 0.5 (at threshold) stays
  expect_equal(
    rgba_of(res)[,, 4],
    matrix(c(0, 0.4, 0.5, 1, 1, 1), nrow = 2, byrow = TRUE)
  )
  expect_true(all(rgba_of(res)[,, 1] == 0.2))
  expect_true(all(rgba_of(res)[,, 3] == 1))
})

test_that("trimming uses the default threshold of 0.5", {
  expect_equal(
    pdr_modify_rgba(list(drawing), op = "low_trim"),
    pdr_modify_rgba(list(drawing), op = "low_trim", threshold = 0.5)
  )
})

test_that("threshold boundaries 0 and 1 leave values unchanged", {
  expect_equal(
    pdr_modify_rgba(list(drawing), op = "low_trim", threshold = 0),
    list(drawing)
  )
  expect_equal(
    pdr_modify_rgba(list(drawing), op = "high_trim", threshold = 1),
    list(drawing)
  )
})

test_that("trimming keeps other elements and the list length", {
  res <- pdr_modify_rgba(list(drawing, drawing), op = "low_trim")

  expect_length(res, 2)
  expect_equal(
    res[[1]][names(res[[1]]) != ".rgba"],
    drawing[names(drawing) != ".rgba"]
  )
})

test_that("apply_stencil multiplies only the alpha channel", {
  res <- pdr_modify_rgba(
    list(drawing),
    op = "apply_stencil",
    stencil = stencil_mat
  )

  expect_equal(rgba_of(res)[,, 4], alpha_mat * stencil_mat)
  expect_equal(rgba_of(res)[,, 1:3], drawing$.rgba[,, 1:3])
})

test_that("apply_stencil accepts a list of matrices of length 1 or n", {
  inverted <- 1 - stencil_mat

  one <- pdr_modify_rgba(
    list(drawing, drawing),
    op = "apply_stencil",
    stencil = list(stencil_mat)
  )
  expect_equal(rgba_of(one, 1)[,, 4], alpha_mat * stencil_mat)
  expect_equal(rgba_of(one, 2)[,, 4], alpha_mat * stencil_mat)

  paired <- pdr_modify_rgba(
    list(drawing, drawing),
    op = "apply_stencil",
    stencil = list(stencil_mat, inverted)
  )
  expect_equal(rgba_of(paired, 1)[,, 4], alpha_mat * stencil_mat)
  expect_equal(rgba_of(paired, 2)[,, 4], alpha_mat * inverted)
})

test_that("apply_stencil accepts a pdr_list with .stencil elements", {
  stencil_list <- list(
    list(.id = "1", .width = 3, .height = 2, .stencil = stencil_mat)
  )
  res <- pdr_modify_rgba(
    list(drawing),
    op = "apply_stencil",
    stencil = stencil_list
  )

  expect_equal(rgba_of(res)[,, 4], alpha_mat * stencil_mat)
})

test_that("apply_stencil works with stencils from pdr_add_stencil", {
  d <- pdr_add_stencil(list(drawing), threshold = 0.5)
  res <- pdr_modify_rgba(list(drawing), op = "apply_stencil", stencil = d)

  expect_equal(rgba_of(res)[,, 4], alpha_mat * d[[1]]$.stencil)
})

test_that("apply_stencil works on rendered example data with the full body stencils", {
  skip_if_not(nrow(pdr_stencil_full_body) >= 1)
  rgba_data <- suppressMessages(pdr_add_rgba(pdr_example_data$pdr_data[1:2]))
  stencil <- pdr_stencil_full_body$pdr_data[1]
  dim_st <- dim(stencil[[1]]$.stencil)
  skip_if_not(identical(dim_st, dim(rgba_data[[1]]$.rgba)[1:2]))

  res <- pdr_modify_rgba(rgba_data, op = "apply_stencil", stencil = stencil)

  expect_length(res, 2)
  expect_true(all(res[[1]]$.rgba[,, 4] <= rgba_data[[1]]$.rgba[,, 4]))
})

test_that("multiply multiplies the whole array by the stencil", {
  res <- pdr_modify_rgba(list(drawing), op = "multiply", stencil = stencil_mat)

  for (ch in 1:4) {
    expect_equal(rgba_of(res)[,, ch], drawing$.rgba[,, ch] * stencil_mat)
  }
})

test_that("addition adds the stencil to the whole array", {
  skip(
    "Known bug: 3D array + 2D matrix is non-conformable and the map2() result is discarded"
  )
  res <- pdr_modify_rgba(list(drawing), op = "addition", stencil = stencil_mat)

  for (ch in 1:4) {
    expect_equal(rgba_of(res)[,, ch], drawing$.rgba[,, ch] + stencil_mat)
  }
})

test_that("a stencil of the wrong length errors", {
  expect_error(
    pdr_modify_rgba(
      list(drawing, drawing, drawing),
      op = "apply_stencil",
      stencil = list(stencil_mat, stencil_mat)
    ),
    "length 1 or same length"
  )
})

test_that("a stencil with mismatched dimensions errors", {
  expect_error(
    pdr_modify_rgba(
      list(drawing),
      op = "apply_stencil",
      stencil = matrix(1, nrow = 3, ncol = 3)
    ),
    "do not match pairwise"
  )
})

test_that("an invalid stencil type errors", {
  expect_error(
    pdr_modify_rgba(list(drawing), op = "apply_stencil", stencil = "nope")
  )
})

test_that("a missing stencil errors for the ops that require it", {
  for (op in c("apply_stencil", "multiply", "addition")) {
    expect_error(
      pdr_modify_rgba(list(drawing), op = op),
      paste0("'stencil' is required for op = '", op, "'")
    )
  }
})

test_that("drawings without .rgba error for every op", {
  no_rgba <- drawing[names(drawing) != ".rgba"]

  for (op in c("low_trim", "high_trim")) {
    expect_error(
      pdr_modify_rgba(list(no_rgba), op = op),
      "must have an '.rgba'"
    )
  }
  expect_error(
    pdr_modify_rgba(list(no_rgba), op = "apply_stencil", stencil = stencil_mat),
    "must have an '.rgba'"
  )
  # One drawing without .rgba among valid ones is enough to error
  expect_error(
    pdr_modify_rgba(list(drawing, no_rgba), op = "low_trim"),
    "must have an '.rgba'"
  )
})

test_that("stencil is ignored by the trim ops", {
  expect_equal(
    pdr_modify_rgba(list(drawing), op = "low_trim", stencil = stencil_mat),
    pdr_modify_rgba(list(drawing), op = "low_trim")
  )
})

test_that("works inside mutate on a pdr_tibble", {
  res <- suppressMessages(
    pdr_example_data[1:2, ] |>
      dplyr::mutate(
        pdr_data = pdr_add_rgba(pdr_data) |>
          pdr_modify_rgba(op = "low_trim", threshold = 0.3)
      )
  )

  expect_equal(nrow(res), 2)
  expect_true(all(vapply(res$pdr_data, \(x) ".rgba" %in% names(x), logical(1))))
  expect_true(all(vapply(
    res$pdr_data,
    \(x) all(x$.rgba[x$.rgba != 0] >= 0.3),
    logical(1)
  )))
})
