# Convert MIRD pain drawings to a pdr_list

MIRD exports pain drawings in a custom format like:
`(,1,2,5,4,),(,8,8,7,7,)` which corresponds to:

## Usage

``` r
pdr_mird2pdr(
  mird_pd,
  id = seq_along(mird_pd),
  file = "",
  version = "",
  width = as.integer(450),
  height = as.integer(500),
  units = "px",
  timestamp = "",
  app = "",
  tool = "pen",
  draw_input_type = "mouse",
  tool_width = as.integer(1),
  point_density = as.integer(1),
  spray_radius = as.integer(1),
  alpha = as.numeric(1),
  color = "#ff0000",
  flipy = TRUE
)
```

## Arguments

- mird_pd:

  A character vector of MIRD pain drawings.

- id:

  A unique id for each drawing (default: `seq_along(mird_pd)`).

- file:

  File name (default `""`).

- version:

  Version (default `""`).

- width:

  Canvas width, integer (default `450L`).

- height:

  Canvas height, integer (default `500L`).

- units:

  Units (default `"px"`).

- timestamp:

  Timestamp (default `""`).

- app:

  App name (default `""`).

- tool:

  Drawing tool (default `"pen"`).

- draw_input_type:

  Input type (default `"mouse"`).

- tool_width:

  Tool width, integer (default `1L`).

- point_density:

  Point density, integer (default `1L`).

- spray_radius:

  Spray radius, integer (default `1L`).

- alpha:

  Alpha, numeric between 0 and 1 (default `1.0`).

- color:

  Stroke colour (default `"#ff0000"`).

- flipy:

  Logical (default `TRUE`). If `TRUE`, the y-axis is flipped (via
  [`pdr_modify_points()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_points.md)
  with `op = "flipy"`), because MIRD has its origin in the upper left
  corner whereas paindrawr uses the lower left.

## Value

A pdr_list (see
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
of length `length(mird_pd)`.

## Details

|     |     |       |
|-----|-----|-------|
| x   | y   | index |
| 1   | 2   | 1     |
| 5   | 4   | 1     |
| 8   | 8   | 2     |
| 7   | 7   | 2     |

This function takes a vector of length n of such MIRD strings and
converts it into a pdr_list of length n (see
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the terminology). Strings that are `NA` give an `NA` element (NA in,
NA out); strings too short to hold a point give a pdr_drawing without
points and strokes.

The intended use case is a data frame (tibble) of MIRD data including
pain drawing data. A new pdr_list column can be generated with a simple
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
call.

Each of the parameters supplied (except `mird_pd`) must be either of the
same length as `mird_pd` or of length 1, in which case it is repeated.

## Examples

``` r
mird <- c(
  "(,460,709,463,709,464,719,448,720,449,712,)",
  "(,393,682,430,672,429,682,)",
  NA
)
pdr_list <- pdr_mird2pdr(mird, id = c("a", "b", "c"))
pdr_list[[1]]$.points
#> # A tibble: 5 × 3
#>      .x    .y .index
#>   <int> <int>  <int>
#> 1   460  -209      1
#> 2   463  -209      1
#> 3   464  -219      1
#> 4   448  -220      1
#> 5   449  -212      1

# Typical use: create a pdr_list column in a tibble
tibble::tibble(id = c("a", "b", "c"), mird = mird) |>
  dplyr::mutate(pdr_data = pdr_mird2pdr(mird, id = id))
#> # A tibble: 3 × 3
#>   id    mird                                        pdr_data         
#>   <chr> <chr>                                       <list>           
#> 1 a     (,460,709,463,709,464,719,448,720,449,712,) <named list [10]>
#> 2 b     (,393,682,430,672,429,682,)                 <named list [10]>
#> 3 c     NA                                          <lgl [1]>        
```
