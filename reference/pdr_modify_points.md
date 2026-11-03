# Modify the points of pain drawings

Performs an operation on the `.points` of each pdr_drawing in a pdr_list
and returns a pdr_list of the same length. It is thus suited for use
inside `mutate()`. See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the definitions of pdr_list and pdr_drawing.

## Usage

``` r
pdr_modify_points(pdr_list, op = "flipy")
```

## Arguments

- pdr_list:

  A pdr_list (see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)),
  e.g. the `pdr_data` column of a pdr_tibble.

- op:

  A string specifying the operation. Currently one of:

  - `"flipy"` (default) — flips the y-coordinates (`.y` becomes
    `.height - .y`), which is useful to move the origin between the top
    and bottom of the canvas. Drawings without `.points` are left
    unchanged.

## Value

A pdr_list of the same length as `pdr_list`, with the modified
`.points`.

## See also

[`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md)

## Examples

``` r
# Flip the y-coordinates and store in a new pdr_list column
pdr_example_data |>
  dplyr::mutate(pdr_data_flipped = pdr_modify_points(pdr_data, "flipy"))
#> # A tibble: 50 × 7
#>    id                   pdr_data     sex     age duration  pain pdr_data_flipped
#>    <chr>                <list>       <fct> <dbl> <fct>    <dbl> <list>          
#>  1 6d63804892233bbb18b… <named list> Fema…  46   Chronic      6 <named list>    
#>  2 c8b12cd45d08e94241d… <named list> Fema…  35.4 Acute        6 <named list>    
#>  3 25062ac7dbeb08d8b53… <named list> Fema…  38.4 Acute        5 <named list>    
#>  4 0dc657fa6ed70150f95… <named list> Male   98   Subacute     3 <named list>    
#>  5 fb61c33ffa09270a5c7… <named list> Fema…  58.3 Acute        1 <named list>    
#>  6 328d31503202db3e5c1… <named list> Male   68.9 Subacute     7 <named list>    
#>  7 e07d8d7e7034ba3ae7f… <named list> Fema…  67.4 Subacute     6 <named list>    
#>  8 9996f1a95c8e939c6ab… <named list> Fema…  53.4 Chronic      9 <named list>    
#>  9 41aa308e694ad270b7c… <named list> Fema…  36.8 Chronic      9 <named list>    
#> 10 0bcfd36a71b799e857f… <named list> Fema…  34.9 Subacute     4 <named list>    
#> # ℹ 40 more rows
```
