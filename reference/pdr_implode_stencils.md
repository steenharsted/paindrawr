# Implode the stencils of multiple pain drawings into one

Collapses the `.stencil` elements of several pain drawings into a single
stencil, by taking the union of the 1-regions of each `.stencil`. This
is useful, for instance, for combining several anatomical region
stencils into one larger region stencil.

## Usage

``` r
pdr_implode_stencils(pdr_list, id = NULL)
```

## Arguments

- pdr_list:

  A pdr_list (see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
  where every pdr_drawing has a `.stencil` element (see
  [`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md)).
  All drawings must have identical `.width`, `.height` and `.units`.

- id:

  The `.id` of the resulting pdr_drawing. If `NULL` (default),
  `"collapsed_<time>"` is used.

## Value

A pdr_list of length 1 with a single pdr_drawing whose `.stencil` is the
union of the input stencils and whose `.stencil_inverted` is its
complement. `.points` and `.strokes` are empty.

## Details

The function is intended for use inside
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
where the result should be a single drawing per group. All input
drawings must have `.stencil` elements (see
[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md))
and identical canvas dimensions and `.units`.

The result holds no strokes or points – it is a stencil, not a drawing.
`.file`, `.version` and `.app` keep the common value if unique,
otherwise `"multiple"` (or `NA` for `.version`). `.timestamp` is the
maximum of the inputs (`NA` if all inputs are `NA`). The collapsed
stencil is also stored in the alpha channel of `.rgba` (as a black
image), so that
[`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)
works on the result.

## See also

[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md),
[`pdr_implode()`](https://steenharsted.github.io/paindrawr/reference/pdr_implode.md),
[`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)

## Examples

``` r
# \donttest{
pdr_stencil_full_body |>
  dplyr::summarise(pdr_data = pdr_implode_stencils(pdr_data))
#> # A tibble: 1 × 1
#>   pdr_data         
#>   <list>           
#> 1 <named list [13]>

pdr_stencil_full_body |>
  dplyr::mutate(
    view = ifelse(stringr::str_detect(id, "^Back_|^Mid_back"), "Back", "Front")
  ) |>
  dplyr::summarise(
    pdr_data = pdr_implode_stencils(pdr_data),
    .by = view
  )
#> # A tibble: 2 × 2
#>   view  pdr_data         
#>   <chr> <list>           
#> 1 Back  <named list [13]>
#> 2 Front <named list [13]>
# }
```
