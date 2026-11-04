# Modify the rgba data of pain drawings

Modifies the `.rgba` array of each pdr_drawing in a pdr_list, e.g. to
apply a stencil that restricts a drawing to an anatomical region, or to
trim low/high alpha values. It returns a pdr_list of the same length and
is thus suited for use inside
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html). The
drawings must already have an `.rgba` element (see
[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md)).
See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the definitions of pdr_list and pdr_drawing.

## Usage

``` r
pdr_modify_rgba(pdr_list, op = "", stencil = NULL, threshold = 0.5)
```

## Arguments

- pdr_list:

  A pdr_list (see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
  whose pdr_drawings contain `.rgba`, e.g. the `pdr_data` column of a
  pdr_tibble.

- op:

  A string specifying the operation. There is no default (the default
  `""` raises an error); one of:

  - `"apply_stencil"` — multiplies the alpha channel (4th layer) of
    `.rgba` by `stencil`, so that pixels outside the stencil become
    fully transparent. Requires `stencil`.

  - `"multiply"` — multiplies the whole `.rgba` array by `stencil`.
    Requires `stencil`.

  - `"addition"` — **unfinished, do not use.** Intended to add `stencil`
    to the whole `.rgba` array, but it currently errors and its
    weighting is still to be decided. Requires `stencil`.

  - `"low_trim"` — sets all `.rgba` values below `threshold` to 0.

  - `"high_trim"` — sets all `.rgba` values above `threshold` to 1.

- stencil:

  The stencil(s) used by the ops `"apply_stencil"`, `"multiply"` and
  `"addition"`; otherwise ignored. Either

  - a single matrix,

  - a list of matrices, or

  - a pdr_list in which every pdr_drawing has a `.stencil` element (see
    [`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md)),
    e.g. `pdr_stencil_full_body$pdr_data`.

  The length must be 1 (the stencil is used for every drawing) or equal
  to the length of `pdr_list` (stencils are paired with drawings by
  position). The height and width of each stencil must match the `.rgba`
  of the drawing it is paired with. Default is `NULL`.

- threshold:

  A number between 0 and 1 used by `"low_trim"` and `"high_trim"`;
  otherwise ignored. Default is `0.5`.

## Value

A pdr_list of the same length as `pdr_list`, with modified `.rgba`
elements.

## See also

[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md),
[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md),
[`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)

## Examples

``` r
# Keep only the pain drawn within the first region of the full body stencils
pdr_example_data[1:2, ] |>
  dplyr::mutate(
    pdr_data = pdr_add_rgba(pdr_data) |>
      pdr_modify_rgba(
        op = "apply_stencil",
        stencil = pdr_stencil_full_body$pdr_data[1]
      )
  ) |>
  pdr_plot_rgba()


# Remove faint (low alpha) pixels
pdr_example_data[1:2, ] |>
  dplyr::mutate(
    pdr_data = pdr_add_rgba(pdr_data) |>
      pdr_modify_rgba(op = "low_trim", threshold = 0.3)
  )
#> # A tibble: 2 × 6
#>   id                               pdr_data          sex      age duration  pain
#>   <chr>                            <list>            <fct>  <dbl> <fct>    <dbl>
#> 1 6d63804892233bbb18bc21bcf0d81d0d <named list [11]> Female  46   Chronic      6
#> 2 c8b12cd45d08e94241d4de9990d60f71 <named list [11]> Female  35.4 Acute        6
```
