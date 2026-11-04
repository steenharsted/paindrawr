# Example hand drawings of imagined pain (palmar view)

A small, deliberately silly data set collected to demonstrate the
PainDraw and paindrawr workflows. Participants first chose between a
hammer and an apple, and then a distance (10 cm or 1 m). They were then
told that the chosen object had fallen from that height and landed on
their left hand, and were asked to draw their imaginary hand pain on a
palmar lower arm template.

## Usage

``` r
pdr_example_palmar
```

## Format

A tibble with 30 rows and 4 columns:

- id:

  Integer. Participant (record) identifier.

- palmar:

  List column with one drawing per row (`NULL` if missing).

- tool:

  Factor. Object that fell on the hand: `"Apple"` or `"Hammer"`.

- distance:

  Factor. Height the object fell from: `"10 centimeters"` or
  `"1 meter"`.

## Source

Collected by the package authors; created in
`data-raw/pdr_example_palmar.R`.

## Details

One participant (`id` 22) has a missing drawing (and missing `tool` and
`distance`). The drawing is `NULL`, which is useful for demonstrating
how missing drawings are handled as opposed to empty drawings. File
paths and timestamps have been removed from the drawings. Use
[`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md)
to rasterise the drawings. The matching background image is
`bg_lower_arm_palmar.png` in
`system.file("extdata", package = "paindrawr")`.
