# Synthetic 2 x 3 px drawing; alpha: 0, 0.4, 0.5 / 0.6, 1, 1
make_drawing <- function(a, id = "1") {
  rgba <- array(0, dim = c(2, 3, 4))
  rgba[,, 4] <- a
  list(.id = id, .width = 3, .height = 2, .rgba = rgba)
}

alpha_mat <- matrix(c(0, 0.4, 0.5, 0.6, 1, 1), nrow = 2, byrow = TRUE)
drawn <- c(0.4, 0.5, 0.6, 1, 1) # non-zero alpha values

make_tbl <- function(...) {
  tibble::tibble(
    id = 1:2,
    pdr_data = list(make_drawing(alpha_mat), make_drawing(1))
  )
}
tbl <- make_tbl()

# Stencil keeping only the first column (left) / last two columns
make_stencil <- function(m, id) {
  list(.id = id, .stencil = m, .width = 3, .height = 2)
}
st_left <- make_stencil(matrix(c(1, 0, 0, 1, 0, 0), 2, byrow = TRUE), "left")
st_right <- make_stencil(matrix(c(0, 1, 1, 0, 1, 1), 2, byrow = TRUE), "right")

test_that("default returns the seven statistics, keeping existing columns", {
  res <- pdr_summarize_rgba(tbl)

  expect_equal(nrow(res), 2)
  expect_equal(names(res)[1:2], c("id", "pdr_data"))
  expect_equal(
    names(res)[-(1:2)],
    paste0("pdr_data_", c("px", "mean", "sd", "median", "iqr", "min", "max"))
  )
})

test_that("statistics are computed on drawn pixels only", {
  res <- pdr_summarize_rgba(tbl)

  expect_equal(res$pdr_data_px, c(5, 6))
  expect_equal(res$pdr_data_mean[1], mean(drawn))
  expect_equal(res$pdr_data_sd[1], sd(drawn))
  expect_equal(res$pdr_data_median[1], median(drawn))
  expect_equal(res$pdr_data_iqr[1], IQR(drawn))
  expect_equal(res$pdr_data_min[1], 0.4)
  expect_equal(res$pdr_data_max[1], 1)

  # Fully opaque drawing
  expect_equal(res$pdr_data_mean[2], 1)
  expect_equal(res$pdr_data_sd[2], 0)
})

test_that("area_prop is px divided by width * height", {
  res <- pdr_summarize_rgba(tbl, summary_stat = c("px", "area_prop"))

  expect_equal(res$pdr_data_area_prop, res$pdr_data_px / 6)
  expect_equal(res$pdr_data_area_prop, c(5 / 6, 1))
})

test_that("quantile statistics match stats::quantile", {
  qs <- c(
    q1 = 0.01,
    q5 = 0.05,
    q10 = 0.1,
    q25 = 0.25,
    q75 = 0.75,
    q90 = 0.9,
    q95 = 0.95,
    q99 = 0.99
  )
  res <- pdr_summarize_rgba(tbl, summary_stat = names(qs))

  for (nm in names(qs)) {
    expect_equal(
      unname(res[[paste0("pdr_data_", nm)]][1]),
      unname(quantile(drawn, qs[[nm]])),
      info = nm
    )
  }
})

test_that("'all' returns fifteen statistics and excludes q10", {
  res <- pdr_summarize_rgba(tbl, summary_stat = "all")
  stats <- sub("^pdr_data_", "", setdiff(names(res), c("id", "pdr_data")))

  expect_length(stats, 15)
  expect_true("area_prop" %in% stats)
  expect_false("q10" %in% stats)
})

test_that("columns are returned in fixed order regardless of request order", {
  res <- pdr_summarize_rgba(tbl, summary_stat = c("mean", "px"))

  expect_equal(names(res)[-(1:2)], c("pdr_data_px", "pdr_data_mean"))
})

test_that("alpha_range is exclusive below and inclusive above", {
  res <- suppressWarnings(pdr_summarize_rgba(
    tbl,
    summary_stat = c("px", "min", "max"),
    alpha_range = c(0.4, 0.6)
  ))

  # Keeps 0.5 and 0.6 only in drawing 1; none in drawing 2 (all 1)
  expect_equal(res$pdr_data_px[1], 2)
  expect_equal(res$pdr_data_min[1], 0.5)
  expect_equal(res$pdr_data_max[1], 0.6)

  res_hi <- pdr_summarize_rgba(
    tbl,
    summary_stat = "px",
    alpha_range = c(0.5, 1)
  )
  expect_equal(res_hi$pdr_data_px, c(3, 6))
})

test_that("no pixels in range gives px 0 and NA/Inf statistics", {
  res <- suppressWarnings(
    pdr_summarize_rgba(
      tbl[2, ],
      summary_stat = c("px", "mean", "median", "sd", "q25", "min", "max"),
      alpha_range = c(0, 0.5)
    )
  )

  expect_equal(res$pdr_data_px, 0)
  expect_true(is.na(res$pdr_data_mean))
  expect_true(is.na(res$pdr_data_median))
  expect_true(is.na(res$pdr_data_sd))
  expect_true(is.na(res$pdr_data_q25))
  expect_equal(res$pdr_data_min, Inf)
  expect_equal(res$pdr_data_max, -Inf)
})

test_that("NA alpha values are ignored", {
  a <- alpha_mat
  a[1, 1] <- NA
  d <- tibble::tibble(pdr_data = list(make_drawing(a)))
  res <- pdr_summarize_rgba(d, summary_stat = c("px", "mean"))

  expect_equal(res$pdr_data_px, 5)
  expect_equal(res$pdr_data_mean, mean(drawn))
})

test_that("missing drawings give NA for every statistic, including px", {
  stats <- c(
    "px",
    "area_prop",
    "mean",
    "sd",
    "median",
    "iqr",
    "min",
    "max",
    "q25"
  )
  all_na_alpha <- make_drawing(NA_real_)
  na_rgba <- modifyList(make_drawing(1), list(.rgba = NA))

  d <- tibble::tibble(
    pdr_data = list(make_drawing(alpha_mat), NA, all_na_alpha, na_rgba)
  )
  res <- pdr_summarize_rgba(d, summary_stat = stats)

  expect_equal(nrow(res), 4)
  expect_no_warning(pdr_summarize_rgba(d, summary_stat = stats))
  expect_false(anyNA(res[1, -1]))
  expect_true(all(is.na(res[2:4, -1])))
  expect_equal(res$pdr_data_px[1], 5)

  # With stencils too
  res_st <- pdr_summarize_rgba(d, summary_stat = "px", stencils = list(st_left))
  expect_equal(res_st$pdr_data_left_px, c(1, NA, NA, NA))
})

test_that("pdr_col accepts unquoted names, strings and several columns", {
  tbl2 <- dplyr::mutate(tbl, other = pdr_data)

  a <- pdr_summarize_rgba(tbl2, pdr_col = other, summary_stat = "px")
  b <- pdr_summarize_rgba(tbl2, pdr_col = "other", summary_stat = "px")
  expect_equal(a, b)
  expect_true("other_px" %in% names(a))
  expect_false("pdr_data_px" %in% names(a))

  both <- pdr_summarize_rgba(
    tbl2,
    pdr_col = c(pdr_data, other),
    summary_stat = "px"
  )
  expect_true(all(c("pdr_data_px", "other_px") %in% names(both)))
  expect_equal(both$pdr_data_px, both$other_px)
})

test_that("stencils give one set of columns per stencil", {
  res <- pdr_summarize_rgba(
    tbl,
    summary_stat = c("px", "mean"),
    stencils = list(st_left, st_right)
  )

  expect_equal(
    names(res)[-(1:2)],
    c(
      "pdr_data_left_px",
      "pdr_data_left_mean",
      "pdr_data_right_px",
      "pdr_data_right_mean"
    )
  )
  # Left column alpha: 0, 0.6 -> only 0.6 is drawn
  expect_equal(res$pdr_data_left_px, c(1, 2))
  expect_equal(res$pdr_data_left_mean[1], 0.6)
  # Right columns alpha: 0.4, 0.5, 1, 1
  expect_equal(res$pdr_data_right_px, c(4, 4))
  expect_equal(res$pdr_data_right_mean[1], mean(c(0.4, 0.5, 1, 1)))
})

test_that("works with stencils built by pdr_add_stencil and the full-body set", {
  st <- pdr_add_stencil(list(make_drawing(alpha_mat)), threshold = 0.5)
  st[[1]]$.id <- "region"
  res <- pdr_summarize_rgba(tbl, summary_stat = "px", stencils = st)

  expect_true("pdr_data_region_px" %in% names(res))

  ex <- suppressMessages(
    pdr_example_data[1:2, ] |>
      dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
  )
  full <- pdr_summarize_rgba(
    ex,
    summary_stat = "px",
    stencils = pdr_stencil_full_body$pdr_data[1:2]
  )
  expect_equal(ncol(full), ncol(ex) + 2)
})

test_that("invalid alpha_range errors", {
  msg <- "alpha_range"
  expect_error(pdr_summarize_rgba(tbl, alpha_range = 0.5), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c(0, 0.5, 1)), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c(-0.1, 1)), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c(0, 1.1)), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c(0.5, 0.5)), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c(0.8, 0.2)), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c(NA, 1)), msg)
  expect_error(pdr_summarize_rgba(tbl, alpha_range = c("0", "1")), msg)
})

test_that("missing .rgba errors", {
  expect_error(
    pdr_summarize_rgba(pdr_example_data[1:2, ]),
    "\\.rgba"
  )

  # One drawing lacking .rgba is enough
  bad <- tbl
  bad$pdr_data[[2]]$.rgba <- NULL
  expect_error(pdr_summarize_rgba(bad), "\\.rgba")
})

test_that("invalid stencils error", {
  expect_error(pdr_summarize_rgba(tbl, stencils = "nope"))
  expect_error(pdr_summarize_rgba(tbl, stencils = 1:3))
})
