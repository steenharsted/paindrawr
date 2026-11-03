# Plot RGBA rasters in a grid

Plots the `.rgba` raster arrays stored in the pdr_list column of a
pdr_tibble as a ggplot, one facet per row of `.data`, using
[`ggplot2::annotation_raster()`](https://ggplot2.tidyverse.org/reference/annotation_raster.html).
Optionally draws a background image beneath each raster. See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the definitions of pdr_tibble, pdr_list and pdr_drawing.

## Usage

``` r
pdr_plot_rgba(
  .data,
  pdr_col = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  only_alpha = FALSE,
  ncol = NULL,
  nrow = NULL
)
```

## Arguments

- .data:

  A pdr_tibble (see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
  whose pdr_drawings contain an `.rgba` element. Add it first with
  [`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md).

- pdr_col:

  The pdr_list column in `.data` holding the pain drawings (tidy-eval,
  unquoted). Defaults to `pdr_data`.

- background_image:

  Optional background drawn beneath each drawing. Either a path to a PNG
  file (character string ending in `.png`) or a numeric raster array as
  returned by
  [`png::readPNG()`](https://rdrr.io/pkg/png/man/readPNG.html). If
  `NULL` (default), no background is drawn. Use
  [`pdr_example_file()`](https://steenharsted.github.io/paindrawr/reference/pdr_example_file.md)
  to get the path to a bundled background.

- include_id:

  Logical. If `TRUE`, the `.id` of each drawing is shown as a facet
  strip label above each panel. Default is `FALSE`.

- only_alpha:

  Logical. If `TRUE`, pixels with alpha \> 0 are drawn opaque black and
  pixels with alpha == 0 are drawn white (still transparent), to show
  which pixels have been drawn on. Default is `FALSE`.

- ncol, nrow:

  Number of columns and rows, passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  If both are `NULL` (default), `facet_wrap()` chooses the layout.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object with one facet per drawing.

## See also

[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md),
[`pdr_plot_rgba_heatmap()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba_heatmap.md),
[`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)

## Examples

``` r
pdr_example_data[1, ] |>
  dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data)) |>
  pdr_plot_rgba()


# \donttest{
pdr_example_data[1:4, ] |>
  dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data)) |>
  pdr_plot_rgba(
    background_image = pdr_example_file("bg_full_body.png"),
    include_id = TRUE,
    ncol = 2
  )

# }
```
