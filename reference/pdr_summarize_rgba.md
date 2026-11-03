# Summarise the alpha values of drawn pixels in a pain drawing

Computes summary statistics of the alpha channel of the `.rgba` array of
each drawing, using only pixels whose alpha falls within `alpha_range`
(lower bound exclusive, upper bound inclusive). The number of drawn
pixels (`px`) and the proportion of the canvas covered (`area_prop`)
describe how much was drawn, while e.g. `mean` or `median` describe how
dark or opaque the strokes are. Optionally, each drawing is first
multiplied by one or more stencils (e.g. body regions) so that
statistics are computed per region.

## Usage

``` r
pdr_summarize_rgba(
  .data,
  pdr_col = pdr_data,
  summary_stat = "seven",
  alpha_range = c(0, 1),
  stencils = NULL
)
```

## Arguments

- .data:

  A pdr_tibble (see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
  whose pdr_drawings contain an `.rgba` element. Add it first with
  [`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md).

- pdr_col:

  \<[`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)\>
  One or more list-columns of `.data` (pdr_lists) with `.rgba` in each
  drawing. Defaults to `pdr_data`. Unquoted names, strings and
  selections such as `c(a, b)` are accepted; each selected column is
  summarised separately.

- summary_stat:

  Character vector of statistics to compute from the alpha values of the
  included pixels. Any of `"px"` (number of pixels), `"area_prop"` (`px`
  divided by `.width * .height`), `"mean"`, `"sd"`, `"median"`, `"iqr"`,
  `"min"`, `"max"`, `"q1"`, `"q5"`, `"q10"`, `"q25"`, `"q75"`, `"q90"`,
  `"q95"` and `"q99"` (the `q` statistics are quantiles at probabilities
  0.01, 0.05, ..., 0.99). Two shortcuts are available when given as a
  single string: `"seven"` (default) is `px`, `mean`, `max`, `min`,
  `median`, `sd` and `iqr`; `"all"` is `seven` plus `area_prop`, `q1`,
  `q5`, `q25`, `q75`, `q90`, `q95` and `q99` (note: `q10` is *not*
  included in `"all"`, but can be requested explicitly). Columns are
  returned in a fixed order, not in the order requested: `px`,
  `area_prop`, `mean`, `sd`, `median`, `iqr`, `min`, `max`, `q1`, `q5`,
  `q10`, `q25`, `q75`, `q90`, `q95`, `q99` (restricted to those
  requested). Note this differs from the order in which `"seven"` is
  listed above.

- alpha_range:

  Numeric vector of length 2 giving the lower (exclusive) and upper
  (inclusive) alpha bounds of pixels to include, with
  `0 <= alpha_range[1] < alpha_range[2] <= 1`. Defaults to `c(0, 1)`,
  i.e. all drawn pixels. Use e.g. `c(0.5, 1)` to restrict to heavily
  drawn pixels.

- stencils:

  Optional pdr_list in which each element has a `.stencil` (a height x
  width matrix of 0/1) and an `.id`, e.g. the list-column
  `pdr_stencil_full_body$pdr_data` or a list built with
  [`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md).
  Each stencil is applied (multiplied) to the alpha channel of every
  drawing before summarising, so the stencil dimensions must match the
  drawings. Defaults to `NULL` (no stencil). Note that this is
  deliberately plural and differs from `stencil` in
  [`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md).

## Value

`.data` (a pdr_tibble) with added numeric columns, one per selected
`pdr_col` and statistic (and per stencil, if `stencils` is given).
Columns are named `<pdr_col>_<stat>` without stencils, and
`<pdr_col>_<stencil id>_<stat>` with stencils, e.g. `pdr_data_mean` or
`pdr_data_Back_head_mean`. If no pixels fall in `alpha_range`, `px` is
0, `mean`, `sd`, `median`, `iqr` and quantiles are `NA`, and `min`/`max`
are `Inf`/`-Inf` (with a warning). If a drawing is missing (`NA`), its
`.rgba` is `NA`, or its alpha channel is entirely `NA`, all statistics
for that drawing, including `px`, are `NA`. Individual `NA` pixels are
ignored.

## Details

The drawings must already contain `.rgba`; see
[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md).
For the terms pdr_drawing, pdr_list and pdr_tibble, see
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md).

## See also

[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md)
to add `.rgba`,
[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md)
and
[`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md)
for stencils,
[`pdr_plot_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba.md)
to plot the arrays.

## Examples

``` r
pd <- pdr_example_data |>
  dplyr::mutate(pdr_data = pdr_add_rgba(pdr_data))
#> `geom_path()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?

# Default: the "seven" statistics across all drawn pixels
pdr_summarize_rgba(pd)
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> # A tibble: 50 × 13
#>    id          pdr_data     sex     age duration  pain pdr_data_px pdr_data_mean
#>    <chr>       <list>       <fct> <dbl> <fct>    <dbl>       <int>         <dbl>
#>  1 6d63804892… <named list> Fema…  46   Chronic      6        1452         0.814
#>  2 c8b12cd45d… <named list> Fema…  35.4 Acute        6        5724         0.827
#>  3 25062ac7db… <named list> Fema…  38.4 Acute        5        1242         0.852
#>  4 0dc657fa6e… <named list> Male   98   Subacute     3        4258         0.815
#>  5 fb61c33ffa… <named list> Fema…  58.3 Acute        1         523         0.841
#>  6 328d315032… <named list> Male   68.9 Subacute     7        3500         0.847
#>  7 e07d8d7e70… <named list> Fema…  67.4 Subacute     6        1297         0.817
#>  8 9996f1a95c… <named list> Fema…  53.4 Chronic      9        2343         0.815
#>  9 41aa308e69… <named list> Fema…  36.8 Chronic      9         895         0.819
#> 10 0bcfd36a71… <named list> Fema…  34.9 Subacute     4        2546         0.828
#> # ℹ 40 more rows
#> # ℹ 5 more variables: pdr_data_sd <dbl>, pdr_data_median <dbl>,
#> #   pdr_data_iqr <dbl>, pdr_data_min <dbl>, pdr_data_max <dbl>

# Selected statistics, only heavily drawn pixels
pdr_summarize_rgba(
  pd,
  summary_stat = c("px", "area_prop", "mean"),
  alpha_range = c(0.5, 1)
)
#> # A tibble: 50 × 9
#>    id     pdr_data     sex     age duration  pain pdr_data_px pdr_data_area_prop
#>    <chr>  <list>       <fct> <dbl> <fct>    <dbl>       <int>              <dbl>
#>  1 6d638… <named list> Fema…  46   Chronic      6        1273            0.00566
#>  2 c8b12… <named list> Fema…  35.4 Acute        6        5058            0.0225 
#>  3 25062… <named list> Fema…  38.4 Acute        5        1088            0.00484
#>  4 0dc65… <named list> Male   98   Subacute     3        3774            0.0168 
#>  5 fb61c… <named list> Fema…  58.3 Acute        1         469            0.00208
#>  6 328d3… <named list> Male   68.9 Subacute     7        3178            0.0141 
#>  7 e07d8… <named list> Fema…  67.4 Subacute     6        1134            0.00504
#>  8 9996f… <named list> Fema…  53.4 Chronic      9        2016            0.00896
#>  9 41aa3… <named list> Fema…  36.8 Chronic      9         763            0.00339
#> 10 0bcfd… <named list> Fema…  34.9 Subacute     4        2209            0.00982
#> # ℹ 40 more rows
#> # ℹ 1 more variable: pdr_data_mean <dbl>

# Per stencil (here, two body regions)
pdr_summarize_rgba(
  pd,
  summary_stat = "px",
  stencils = pdr_stencil_full_body$pdr_data[1:2]
)
#> # A tibble: 50 × 8
#>    id              pdr_data     sex     age duration  pain pdr_data_Back_head_px
#>    <chr>           <list>       <fct> <dbl> <fct>    <dbl>                 <int>
#>  1 6d63804892233b… <named list> Fema…  46   Chronic      6                     0
#>  2 c8b12cd45d08e9… <named list> Fema…  35.4 Acute        6                     0
#>  3 25062ac7dbeb08… <named list> Fema…  38.4 Acute        5                     0
#>  4 0dc657fa6ed701… <named list> Male   98   Subacute     3                     0
#>  5 fb61c33ffa0927… <named list> Fema…  58.3 Acute        1                     0
#>  6 328d31503202db… <named list> Male   68.9 Subacute     7                     0
#>  7 e07d8d7e7034ba… <named list> Fema…  67.4 Subacute     6                     0
#>  8 9996f1a95c8e93… <named list> Fema…  53.4 Chronic      9                     0
#>  9 41aa308e694ad2… <named list> Fema…  36.8 Chronic      9                     0
#> 10 0bcfd36a71b799… <named list> Fema…  34.9 Subacute     4                     0
#> # ℹ 40 more rows
#> # ℹ 1 more variable: pdr_data_Back_left_arm_px <int>
```
