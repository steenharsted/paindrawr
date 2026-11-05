# Plot stencil data

Plots the `.stencil` matrix of each drawing (or `.stencil_inverted` for
the inverted stencil) as a red mask. The function is primarily for
checking stencils created with
[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md).

## Usage

``` r
pdr_plot_stencil(
  .data,
  pdr_col = pdr_data,
  background_image = NULL,
  include_id = FALSE,
  ncol = NULL,
  nrow = NULL,
  invert = FALSE
)
```

## Arguments

- .data:

  A data frame (typically a pdr_tibble, see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
  with a pdr_list column whose pdr_drawings contain a `.stencil` element
  (and `.stencil_inverted` if `invert = TRUE`). The stencil elements are
  added by
  [`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md).

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

- ncol, nrow:

  Number of columns and rows in the plot grid, passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  via
  [`pdr_plot_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba.md).
  If both are `NULL` (default), `facet_wrap()` chooses the layout.

- invert:

  Logical. If `TRUE`, the inverted stencil (`.stencil_inverted`) is
  plotted instead of `.stencil`. Default is `FALSE`.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object with one facet per drawing, showing the stencil.

## Details

It is a wrapper for
[`pdr_plot_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba.md):
for each drawing a red raster is built whose alpha channel is the
stencil, and the result is passed on to
[`pdr_plot_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba.md).
The drawings do not need an `.rgba` element, and `.data` does not need
an `id` column; panels are identified by the `.id` of each drawing.

## See also

[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md),
[`pdr_plot_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba.md)

## Examples

``` r
# \donttest{
pdr_stencil_full_body[1:2, ] |>
  pdr_plot_stencil(pdr_data)


pdr_stencil_full_body[1:2, ] |>
  pdr_plot_stencil(
    pdr_data,
    background_image = pdr_example_file("bg_full_body.png"),
    include_id = TRUE,
    invert = TRUE
  )

# }
```
