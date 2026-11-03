# Anatomical regions of the full body template, with stencils

One drawing per anatomical region of the full body template, each with
`.rgba` and a `.stencil` / `.stencil_inverted` (see
[`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md)).
Intended for use as `stencils` in
[`pdr_summarize_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_summarize_rgba.md)
or as `stencil` in
[`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md).
See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the definition of a pdr_tibble.

## Usage

``` r
pdr_stencil_full_body
```

## Format

A pdr_tibble (tibble) with 46 rows and 2 columns:

- pdr_data:

  pdr_list column with one pdr_drawing per region, including `.rgba`,
  `.stencil` and `.stencil_inverted`.

- id:

  Character. Region name.

## Source

Built from the files in `data-raw/regions_of_full_body_template/` by
`data-raw/DATASET.R`.
