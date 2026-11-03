# Example pain drawings with simple geometric cases

Ten small drawings (e.g. no area, duplicated points) useful for testing
geometry handling. See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the definition of a pdr_tibble.

## Usage

``` r
pdr_example_geometry
```

## Format

A pdr_tibble (tibble) with 10 rows and 2 columns:

- id:

  Character. Name of the geometric case.

- pdr_data:

  pdr_list column with one pdr_drawing per row.

## Source

Created from MIRD-format strings in `data-raw/DATASET.R`.
