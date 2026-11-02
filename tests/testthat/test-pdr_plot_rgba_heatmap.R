# Small synthetic data (4 x 5 px canvas) so pixel values are known exactly.
# Each drawing is described by a list of c(row, col, alpha) marks.
make_rgba <- function(marks = list(), h = 4, w = 5) {
  a <- array(0, dim = c(h, w, 4))
  for (m in marks) {
    a[m[1], m[2], 4] <- m[3]
  }
  a
}

make_pd <- function(marks_list, group = NULL, num = NULL, h = 4, w = 5) {
  n <- length(marks_list)
  tibble::tibble(
    group = group %||% rep("a", n),
    num = num %||% seq_len(n),
    pdr_data = lapply(seq_len(n), \(i) {
      list(
        .id = as.character(i),
        .width = w,
        .height = h,
        .rgba = make_rgba(marks_list[[i]], h, w)
      )
    })
  )
}

# Quiet wrapper: the function reports progress via messages
hm <- function(...) suppressMessages(pdr_plot_rgba_heatmap(...))

# Value of the heatmap at matrix position (row, col); the plot flips rows
cell <- function(p, row, col, grp1 = NULL) {
  d <- p$data
  if (!is.null(grp1)) {
    d <- d[d$.VAR1_grp == grp1, ]
  }
  d$pct[d$.x == col - 0.5 & d$.y == nrow_of(p) - row + 0.5]
}
nrow_of <- function(p) max(p$data$.y) + 0.5

# A: marks (1,1) with 1 and (2,2) with 0.5; B: marks (1,1) with 0.5;
# C: empty drawing
three <- make_pd(
  list(
    list(c(1, 1, 1), c(2, 2, 0.5)),
    list(c(1, 1, 0.5)),
    list()
  )
)

example_rgba <- suppressMessages(
  pdr_example_data |>
    dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
)
bg_png <- pdr_example_file("bg_full_body.png")

test_that("returns a ggplot covering the full pixel grid", {
  p <- hm(three)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 4 * 5)
  expect_equal(p$coordinates$limits$x, c(0, 5))
  expect_equal(p$coordinates$limits$y, c(0, 4))
})

test_that("progress is reported as messages", {
  expect_message(pdr_plot_rgba_heatmap(three), "Validating inputs")
})

test_that("binary value gives the share of drawings marking each pixel", {
  p <- hm(three)

  expect_equal(cell(p, 1, 1), 2 / 3)
  expect_equal(cell(p, 2, 2), 1 / 3)
  # Unmarked cells are NA (transparent)
  expect_equal(sum(!is.na(p$data$pct)), 2)
})

test_that("mean value averages alpha over all drawings in the group", {
  p <- hm(three, value = "mean", alpha_threshold = 0)

  expect_equal(cell(p, 1, 1), (1 + 0.5 + 0) / 3)
  expect_equal(cell(p, 2, 2), 0.5 / 3)
})

test_that("alpha_threshold removes pixels at or below it", {
  p <- hm(three, alpha_threshold = 0.5)

  expect_equal(cell(p, 1, 1), 1 / 3)
  expect_true(is.na(cell(p, 2, 2)))
})

test_that("empty drawings count towards group size", {
  p <- hm(three)
  expect_equal(unique(p$data$n_group), 3)
})

test_that("drawings with NULL or NA .rgba are removed and reported", {
  x <- three
  x$pdr_data[[3]]$.rgba <- NULL

  expect_message(
    pdr_plot_rgba_heatmap(x),
    "1 drawing with NULL/NA"
  )
  p <- hm(x)
  expect_equal(unique(p$data$n_group), 2)
  expect_equal(cell(p, 2, 2), 1 / 2)

  x$pdr_data[[3]]$.rgba <- NA
  expect_equal(unique(hm(x)$data$n_group), 2)
})

test_that("input without .rgba errors with a hint", {
  expect_error(
    hm(pdr_example_data),
    "pdr_add_rgba"
  )
})

test_that("all-missing .rgba errors", {
  x <- three
  x$pdr_data <- lapply(x$pdr_data, \(d) {
    d$.rgba <- NA
    d
  })

  expect_error(hm(x), "NULL or NA")
})

test_that("mixed canvas sizes are rejected", {
  mixed <- dplyr::bind_rows(three, make_pd(list(list()), h = 6, w = 7))

  expect_error(hm(mixed), "single unique")
})

test_that("a custom pdr_col name works", {
  renamed <- dplyr::rename(three, my_col = pdr_data)

  expect_s3_class(hm(renamed, pdr_col = my_col), "ggplot")
  expect_error(hm(three, pdr_col = my_col))
})

# --- Grouping -----------------------------------------------------------------

grouped <- make_pd(
  list(
    list(c(1, 1, 1)),
    list(c(1, 1, 1)),
    list(c(1, 1, 1), c(3, 3, 1)),
    list(c(3, 3, 1))
  ),
  group = c("x", "x", "y", "y"),
  num = c(10, 20, 30, 40)
)

test_that("default is a single unfaceted panel", {
  p <- hm(grouped)

  expect_equal(nlevels(p$data$.VAR1_grp), 1)
  expect_equal(nlevels(p$data$.VAR2_grp), 1)
})

test_that("a categorical variable facets by its levels, normalised per group", {
  p <- hm(grouped, variables = "group")

  expect_setequal(levels(p$data$.VAR1_grp), c("x", "y"))
  expect_equal(cell(p, 1, 1, "x"), 1)
  expect_equal(cell(p, 1, 1, "y"), 1 / 2)
  expect_equal(cell(p, 3, 3, "y"), 1)
  expect_true(is.na(cell(p, 3, 3, "x")))
  expect_equal(
    unique(p$data[c(".VAR1_grp", "n_group")])$n_group,
    c(2, 2)
  )
})

test_that("a numeric variable is binned into quantile groups", {
  p <- hm(grouped, variables = "num", n_groups = 2)

  expect_equal(nlevels(p$data$.VAR1_grp), 2)
  expect_equal(unique(p$data$n_group), 2)
})

test_that("label_format controls numeric group labels", {
  pct <- hm(grouped, variables = "num", label_format = "pct")
  num <- hm(grouped, variables = "num", label_format = "num")
  both <- hm(grouped, variables = "num", label_format = "both")

  expect_true(all(grepl("%", levels(pct$data$.VAR1_grp))))
  expect_false(any(grepl("%", levels(num$data$.VAR1_grp))))
  expect_true(all(grepl("10", levels(num$data$.VAR1_grp)[1])))
  expect_true(all(grepl("%", levels(both$data$.VAR1_grp))))
})

test_that("two variables map to columns and rows of the facet grid", {
  x <- grouped
  x$second <- c("u", "v", "u", "v")
  p <- hm(x, variables = c("group", "second"))

  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(nlevels(p$data$.VAR1_grp), 2)
  expect_equal(nlevels(p$data$.VAR2_grp), 2)
  expect_equal(nrow(unique(p$data[c(".VAR1_grp", ".VAR2_grp")])), 4)
})

test_that("NA in a variables slot leaves that dimension empty", {
  p <- hm(grouped, variables = c(NA, "group"))

  expect_equal(nlevels(p$data$.VAR1_grp), 1)
  expect_setequal(levels(p$data$.VAR2_grp), c("x", "y"))
})

test_that("rows with NA in a grouping variable are dropped", {
  x <- grouped
  x$group[1] <- NA

  expect_message(
    pdr_plot_rgba_heatmap(x, variables = "group"),
    "1 NA value"
  )
  p <- hm(x, variables = "group")
  expect_equal(sum(p$data$.VAR1_grp == "x") / 20, 1)
})

test_that("more than two variables errors", {
  expect_error(hm(grouped, variables = c("a", "b", "c")), "length 1 or 2")
})

# --- Scales -------------------------------------------------------------------

fill_scale <- function(p) {
  p$scales$get_scales("fill")
}

test_that("color_scale = 'max' uses the global maximum", {
  p <- hm(grouped, variables = "group")

  expect_equal(fill_scale(p)$limits, c(0, max(p$data$pct, na.rm = TRUE)))
})

test_that("color_scale = 'facet' rescales each facet to its own maximum", {
  p <- hm(grouped, variables = "group", color_scale = "facet")

  expect_equal(fill_scale(p)$limits, c(0, 1))
  by_facet <- tapply(p$data$pct, p$data$.VAR1_grp, max, na.rm = TRUE)
  expect_equal(as.vector(by_facet), c(1, 1))
})

test_that("a numeric color_scale sets a fixed upper limit", {
  p <- hm(grouped, color_scale = 5)

  expect_equal(fill_scale(p)$limits, c(0, 5))
})

test_that("legend title depends on value and color_scale", {
  title <- \(p) p$guides$guides$fill$params$title

  expect_equal(title(hm(grouped)), "Share of participants")
  expect_equal(title(hm(grouped, value = "mean")), "Mean intensity")
  expect_equal(
    title(hm(grouped, color_scale = "facet")),
    "Relative to facet maximum"
  )
})

test_that("opacity follows alpha_curve and alpha_scale", {
  # Linear curve: opacity equals value / max
  lin <- hm(three, alpha_curve = 1)
  expect_equal(lin$data$.alpha, lin$data$pct / max(lin$data$pct, na.rm = TRUE))

  # Constant opacity
  const <- hm(three, alpha_scale = c(0.4, 0.4))
  expect_true(all(const$data$.alpha == 0.4, na.rm = TRUE))

  # The maximum value reaches the top of alpha_scale
  top <- hm(three, alpha_scale = c(0, 0.7))
  expect_equal(max(top$data$.alpha, na.rm = TRUE), 0.7)
})

test_that("show_n adds a label layer with the group sizes", {
  without <- hm(grouped, variables = "group")
  with <- hm(grouped, variables = "group", show_n = TRUE)

  expect_equal(length(with$layers), length(without$layers) + 1)
  label_layer <- with$layers[[length(with$layers)]]
  expect_s3_class(label_layer$geom, "GeomLabel")
  expect_equal(label_layer$data$label, c("n = 2", "n = 2"))
})

test_that("strips_in_markdown switches the strip text element", {
  md <- hm(grouped, variables = "group")
  plain <- hm(grouped, variables = "group", strips_in_markdown = FALSE)

  expect_s3_class(md$theme$strip.text.x.top, "element_markdown")
  expect_false(inherits(
    plain$theme$strip.text.x.top,
    "ggtext::element_markdown"
  ))
})

test_that("background image adds a raster annotation beneath the heatmap", {
  x <- example_rgba[1:3, ]
  p <- hm(x, background_image = bg_png)
  geoms <- vapply(p$layers, \(l) class(l$geom)[1], "", USE.NAMES = FALSE)

  expect_equal(geoms[1], "GeomRasterAnn")
  expect_true("GeomRaster" %in% geoms)

  expect_equal(
    vapply(hm(x)$layers, \(l) class(l$geom)[1], "", USE.NAMES = FALSE),
    "GeomRaster"
  )
})

# --- Argument validation ------------------------------------------------------

test_that("invalid value and label_format error", {
  expect_error(hm(three, value = "nope"), "must be one of")
  expect_error(hm(three, label_format = "nope"), "must be one of")
})

test_that("invalid alpha_threshold errors", {
  for (bad in list(-0.1, 1, NA_real_, c(0.1, 0.2), "a")) {
    expect_error(hm(three, alpha_threshold = bad), "alpha_threshold")
  }
})

test_that("invalid alpha_scale errors", {
  expect_error(hm(three, alpha_scale = 1), "length 2")
  expect_error(hm(three, alpha_scale = c(-0.1, 1)), "between 0 and 1")
  expect_error(hm(three, alpha_scale = c(0.8, 0.2)), "smaller than or equal")
})

test_that("invalid alpha_curve errors", {
  expect_error(hm(three, alpha_curve = "a"), "alpha_curve")
  expect_error(hm(three, alpha_curve = c(0, 0.5)), "greater than 0")
  expect_error(hm(three, alpha_curve = c(2, 1)), "between 0 and 1")
})

test_that("invalid color_scale errors", {
  for (bad in list("nope", -1, c(1, 2), NA_real_)) {
    expect_error(hm(three, color_scale = bad), "color_scale")
  }
})

test_that("invalid background images give informative errors", {
  expect_error(
    hm(three, background_image = "does_not_exist.png"),
    "does not exist"
  )
  expect_error(
    hm(three, background_image = 1:3),
    "must be a PNG file path or a raster array"
  )
})

test_that("an alpha_scale floor above 0 informs the user", {
  expect_message(
    pdr_plot_rgba_heatmap(three, alpha_scale = c(0.2, 1)),
    "minimum above 0"
  )
})

test_that("a threshold above half the lowest stroke alpha warns", {
  x <- example_rgba[1:3, ]
  x$pdr_data[[1]]$.strokes$.alpha <- 0.1

  # Stroke .alpha is on a 0-1 scale; 0.2 is above half of the lowest (0.1)
  expect_warning(
    suppressMessages(pdr_plot_rgba_heatmap(x, alpha_threshold = 0.2)),
    "above half of the lowest stroke"
  )
  expect_no_warning(hm(x, alpha_threshold = 0.05))
})

# --- Real data ----------------------------------------------------------------

test_that("works on rendered example data with two variables", {
  p <- hm(example_rgba, variables = c("sex", "duration"))

  expect_s3_class(p, "ggplot")
  expect_equal(nlevels(p$data$.VAR1_grp), 2)
  expect_equal(nlevels(p$data$.VAR2_grp), 3)
  expect_true(all(p$data$pct > 0 & p$data$pct <= 1, na.rm = TRUE))
})
