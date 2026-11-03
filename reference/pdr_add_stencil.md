# Create and add a stencil to pain drawings.

Users should only add a stencil to pain drawings that are intended to
serve as stencils for other pain drawings. To apply stencils see
[`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md).

## Usage

``` r
pdr_add_stencil(pdr_list, threshold = 1, channel = "A", by = "<")
```

## Arguments

- pdr_list:

  A pdr_list in which every pdr_drawing has an `.rgba` element (see
  [`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md)
  and
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)).

- threshold:

  A threshold value which determines a pixel as being 0 or 1

- channel:

  A string specifying which channels of the RGBA matrix to compare to
  the threshold.

- by:

  Either '\<' or '\>'

## Value

A pdr_list (see
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
where each pdr_drawing has two added elements: `.stencil`, a numeric
matrix of dimensions height x width with values 0 or 1, and
`.stencil_inverted`, the same matrix with 0 and 1 swapped.

## Details

Pain drawings can be used as stencils and applied to other pain
drawings. This is typically useful, when a particular region of a pain
drawing is of particular interest. E.g. when only a specific anatomical
regions of the pain drawing is relevant.

This function reduces a pain drawing to a stencil – ie. the RGBA data is
reduced to a single level (alpha) with just 0 and 1 values.

The 'threshold' parameter determines the threshold at which a pixel's
value is dichotomized to 0 or 1 (FALSE or TRUE) as per the 'by'
parameter. E.g. a pixel with alpha value 0.4 will evaluate to 0 by '\>'
a 'threshold' of 0.5. With the defaults (`threshold = 1`, `by = "<"`,
`channel = "A"`) a pixel is set to 1 when its alpha is below 1, i.e.
when it is not fully opaque. For a drawing rendered with
[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md),
this is the transparent (undrawn) area, while fully opaque (drawn)
pixels become 0. The inverse is stored in `.stencil_inverted`.

The 'channel' parameter determines which of the R, G, B and A channels
should be evaluated in reference to the threshold. E.g. 'RGBA' evaluates
all of the channels by 'OR', and 'A' evaluates only the alpha channel.

## See also

[`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md),
[`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)

## Examples

``` r
# \donttest{
pdr_example_data[1:3, ] |>
  dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data, polygon = "both")) |>
  dplyr::mutate(pdr_data = pdr_add_stencil(pdr_data, threshold = 0.5, channel = "A", by = "<"))
#> # A tibble: 3 × 6
#>   id                               pdr_data          sex      age duration  pain
#>   <chr>                            <list>            <fct>  <dbl> <fct>    <dbl>
#> 1 6d63804892233bbb18bc21bcf0d81d0d <named list [13]> Female  46   Chronic      6
#> 2 c8b12cd45d08e94241d4de9990d60f71 <named list [13]> Female  35.4 Acute        6
#> 3 25062ac7dbeb08d8b532ac474a7849ca <named list [13]> Female  38.4 Acute        5
# }
```
