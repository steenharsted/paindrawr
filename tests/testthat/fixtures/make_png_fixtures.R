# Generates the small PNG fixtures in fixtures/png/ used by
# test-pdr_import_png.R. Run from the package root:
#   source("tests/testthat/fixtures/make_png_fixtures.R")
#
# All images are tiny (height x width in pixels) so the pixel values tested
# can be read directly from this file. The two real drawings
# (1_palmar_png.png, 2_palmar_png.png) are copies from data-raw/png.

out <- file.path("tests", "testthat", "fixtures", "png")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write <- function(arr, name) png::writePNG(arr, file.path(out, name))

# gray_1ch.png: 2 x 3, grayscale (1 channel), values written column by column
write(
  matrix(c(0, .5, 1, .25, .75, 1), nrow = 2),
  "gray_1ch.png"
)

# gray_alpha_2ch.png: 2 x 2, grayscale + alpha (2 channels)
# Pixel (row 2, col 1) is fully transparent, pixel (row 1, col 2) is half
# transparent, the rest are opaque.
write(
  array(
    c(
      # gray layer
      .2,
      .4,
      .6,
      .8,
      # alpha layer
      1,
      0,
      .5,
      1
    ),
    dim = c(2, 2, 2)
  ),
  "gray_alpha_2ch.png"
)

# rgb_3ch.png: 2 x 3, RGB (3 channels). Column by column:
#   col 1: red, gray (.5)   col 2: white, white   col 3: black, blue
write(
  array(
    c(
      # R layer
      1,
      .5,
      1,
      1,
      0,
      0,
      # G layer
      0,
      .5,
      1,
      1,
      0,
      0,
      # B layer
      0,
      .5,
      1,
      1,
      0,
      1
    ),
    dim = c(2, 3, 3)
  ),
  "rgb_3ch.png"
)

# rgba_transparent_4ch.png: 2 x 2, RGBA (4 channels), all white and opaque,
# except pixel (row 1, col 1), which is transparent white, and pixel
# (row 2, col 2), which is the colour (.2, .4, .6).
local({
  arr <- array(1, dim = c(2, 2, 4))
  arr[1, 1, 4] <- 0
  arr[2, 2, 1:3] <- c(.2, .4, .6)
  write(arr, "rgba_transparent_4ch.png")
})

# rgba_opaque_white_4ch.png: 2 x 2, RGBA, all pixels opaque white
write(array(1, dim = c(2, 2, 4)), "rgba_opaque_white_4ch.png")

# gray_near_white_1ch.png: 2 x 2, grayscale, column by column:
#   col 1: white (1), near white (.9)   col 2: black (0), white (1)
write(
  matrix(c(1, .9, 0, 1), nrow = 2),
  "gray_near_white_1ch.png"
)
