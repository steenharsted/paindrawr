# Example drawings of locations on Zealand

A small, deliberately silly data set collected to demonstrate the
PainDraw and paindrawr workflows. Participants first rated their
knowledge of the geography of East Denmark from 0 ("I think Copenhagen
is in Sweden") to 10 ("I can name every cobblestone in Nyhavn"). They
were then asked to draw one or two circles around the cities Holbæk and
Præstø, as small as possible while still including both cities.

## Usage

``` r
pdr_example_zealand
```

## Format

A tibble with 30 rows and 3 columns:

- id:

  Integer. Participant (record) identifier.

- zealand:

  List column with one drawing per row (`NULL` if missing).

- geography:

  Integer. Self-rated knowledge of the geography of east Denmark, from 0
  to 10; `NA` if not answered.

## Source

Collected by the package authors; created in
`data-raw/pdr_example_zealand.R`.

## Details

One participant (`id` 22) has a missing drawing, stored as `NULL`. File
paths and timestamps have been removed from the drawings. Use
[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md)
(optionally with `polygon = "both"`) to rasterise the drawings. The
matching background image is `bg_zealand.png` in
`system.file("extdata", package = "paindrawr")`.
