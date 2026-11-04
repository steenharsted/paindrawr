# Explode pain drawings into multiple drawings by stroke

Explodes each pdr_drawing into multiple pdr_drawings such that each
stroke (as defined by the `.strokes` and `.points` elements of the
drawing) becomes a new separate pdr_drawing.

## Usage

``` r
pdr_explode(pdr_list)
```

## Arguments

- pdr_list:

  A pdr_list (see
  [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)),
  e.g. the `pdr_data` column of a pdr_tibble.

## Value

A pdr_list where each element is a pdr_drawing containing a single
stroke from the input.

## Details

**NOTE** The output of this function is (likely) not of same length as
input.

For each input pdr_drawing:

- Each row in `.strokes` becomes a new pdr_drawing

- The matching rows (same `.index`) of `.points` are retained

- The `.id` element is modified – see below.

The output is a pdr_list (see
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md))
with a different number of pdr_drawings than the input. The function is
thus not suited for
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
similar functions which expect the same length of input and output.

**Creation of id string**

To ensure unique id strings for all pdr_drawings, the new `.id` has the
format `<index>_<id>_<stroke>`, where `<index>` is the position of the
input pdr_drawing in `pdr_list`, `<id>` is its original `.id`, and
`<stroke>` is the stroke's `.index`. E.g. `"2_abc_3"`.

**Handling of missing or empty data**

A pdr_drawing without strokes (`.strokes` is `NA`, `NULL` or has no
rows) contributes no pdr_drawings to the result.

## Examples

``` r
# Explode a pdr_list into one pdr_drawing per stroke
pdr_exploded <- pdr_explode(pdr_example_data$pdr_data[1:3])

# Number of strokes per original drawing vs. number of exploded drawings
sapply(pdr_example_data$pdr_data[1:3], \(d) nrow(d$.strokes))
#> [1] 2 6 7
length(pdr_exploded)
#> [1] 15

# The new ids have the format <index>_<id>_<stroke>
pdr_get_info(pdr_exploded, ".id")[1:5]
#> [1] "1_6d63804892233bbb18bc21bcf0d81d0d_1"
#> [2] "1_6d63804892233bbb18bc21bcf0d81d0d_2"
#> [3] "2_c8b12cd45d08e94241d4de9990d60f71_1"
#> [4] "2_c8b12cd45d08e94241d4de9990d60f71_2"
#> [5] "2_c8b12cd45d08e94241d4de9990d60f71_3"

# Each pdr_drawing now contains exactly one stroke
nrow(pdr_exploded[[1]]$.strokes)
#> [1] 1
```
