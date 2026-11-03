make_explode_drawing <- function(id = "abc", indices = c(1, 2, 3)) {
  strokes <- tibble::tibble(
    .index = indices,
    .tool = "pen"
  )
  # Stroke k has k points
  points <- purrr::map(indices, \(i) {
    tibble::tibble(.x = seq_len(i), .y = seq_len(i) * 2, .index = i)
  }) |>
    purrr::list_rbind()

  list(
    .id = id,
    .width = 10,
    .height = 20,
    .points = points,
    .strokes = strokes
  )
}

drawing <- make_explode_drawing()

test_that("returns one drawing per stroke", {
  res <- pdr_explode(list(drawing))

  expect_type(res, "list")
  expect_length(res, 3)
})

test_that("output length is the total number of strokes across drawings", {
  d2 <- make_explode_drawing(id = "xyz", indices = c(1, 2))
  res <- pdr_explode(list(drawing, d2))

  expect_length(res, 5)
})

test_that("each exploded drawing contains exactly one stroke and its points", {
  res <- pdr_explode(list(drawing))

  for (k in 1:3) {
    expect_equal(nrow(res[[k]]$.strokes), 1)
    expect_equal(res[[k]]$.strokes$.index, k)
    expect_true(all(res[[k]]$.points$.index == k))
    expect_equal(nrow(res[[k]]$.points), k)
  }
})

test_that("ids have the format <index>_<id>_<stroke>", {
  d2 <- make_explode_drawing(id = "xyz", indices = c(5, 7))
  res <- pdr_explode(list(drawing, d2))

  expect_equal(
    vapply(res, \(d) d$.id, character(1)),
    c("1_abc_1", "1_abc_2", "1_abc_3", "2_xyz_5", "2_xyz_7")
  )
})

test_that("new ids are unique even if input ids are duplicated", {
  res <- pdr_explode(list(drawing, drawing))
  ids <- vapply(res, \(d) d$.id, character(1))

  expect_equal(anyDuplicated(ids), 0L)
})

test_that("non-stroke elements are retained unchanged", {
  res <- pdr_explode(list(drawing))

  expect_equal(res[[1]]$.width, drawing$.width)
  expect_equal(res[[1]]$.height, drawing$.height)
  expect_equal(names(res[[1]]), names(drawing))
})

test_that("stroke and point columns are preserved", {
  res <- pdr_explode(list(drawing))

  expect_equal(names(res[[2]]$.strokes), names(drawing$.strokes))
  expect_equal(names(res[[2]]$.points), names(drawing$.points))
  expect_equal(
    res[[2]]$.points,
    dplyr::filter(drawing$.points, .index == 2)
  )
})

test_that("a drawing with a single stroke gives a single drawing", {
  res <- pdr_explode(list(make_explode_drawing(indices = 4)))

  expect_length(res, 1)
  expect_equal(res[[1]]$.id, "1_abc_4")
})

test_that("empty input gives empty output", {
  expect_length(pdr_explode(list()), 0)
})

test_that("drawings with NULL or zero-row .strokes contribute nothing", {
  d_null <- list(.id = "n", .strokes = NULL, .points = NULL)
  d_empty <- drawing
  d_empty$.strokes <- drawing$.strokes[0, ]
  d_empty$.points <- drawing$.points[0, ]

  expect_length(pdr_explode(list(d_null)), 0)
  expect_length(pdr_explode(list(d_empty)), 0)

  # Mixed with a normal drawing, only the normal one contributes
  res <- pdr_explode(list(d_empty, drawing))
  expect_length(res, 3)
  expect_equal(res[[1]]$.id, "2_abc_1")
})

test_that("drawings with NA .strokes contribute nothing", {
  # Documented behaviour; currently errors ($ on atomic NA) -- see pdr_explode()
  d_na <- list(.id = "na", .strokes = NA, .points = NA)

  expect_length(pdr_explode(list(d_na)), 0)
  expect_length(pdr_explode(list(d_na, drawing)), 3)
})

test_that("works on example data", {
  x <- pdr_example_data$pdr_data[1:3]
  res <- pdr_explode(x)

  n_strokes <- sum(vapply(x, \(d) nrow(d$.strokes), integer(1)))
  expect_length(res, n_strokes)
  expect_true(all(vapply(res, \(d) nrow(d$.strokes) == 1, logical(1))))
  expect_equal(anyDuplicated(pdr_get_info(res, ".id")), 0L)
})
