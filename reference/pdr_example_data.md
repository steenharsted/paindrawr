# Example pain drawings with clinical variables

A simulated data set of 50 pain drawings with accompanying patient
variables. See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for the definition of a pdr_tibble.

## Usage

``` r
pdr_example_data
```

## Format

A pdr_tibble (tibble) with 50 rows and 6 columns:

- id:

  Character. Unique identifier.

- pdr_data:

  pdr_list column with one pdr_drawing per row.

- sex:

  Factor. `"Female"` or `"Male"`.

- age:

  Numeric. Age in years.

- duration:

  Factor. Pain duration category (e.g. `"Subacute"`, `"Chronic"`).

- pain:

  Numeric. Pain intensity rating.

## Source

Simulated; created in `data-raw/DATASET.R`.
