# Check or coerce pain drawing data types

`pdr_match()` matches an input against the three pain drawing data types
used throughout paindrawr, and either reports which type it is (or
whether it is a given type), or coerces it to another type.

## Usage

``` r
pdr_match(input, action = "eval", as = "", mode = "superficial")
```

## Arguments

- input:

  A variable to be matched against the pdr_tibble, pdr_list and
  pdr_drawing types.

- action:

  `"eval"` (default) to evaluate the type of `input`, or `"coerce"` to
  convert `input` to the type given in `as`.

- as:

  One of `""` (default), `"pdr_list"`, `"pdr_tibble"` or
  `"pdr_drawing"`. For `action = "eval"`, the type to test for (`""`
  returns the type as a string). For `action = "coerce"`, the type to
  coerce to.

- mode:

  `"superficial"` (default) for a quick structural check, or
  `"validate"` for a detailed check of structure and content.

## Value

For `action = "eval"`: a string (the type of `input`, or `"invalid"`) if
`as = ""`, otherwise `TRUE`/`FALSE`. For `action = "coerce"`: `input` as
a pdr_tibble, pdr_list or pdr_drawing, as requested in `as`.

## Terminology

This is where the paindrawr data types are defined; all other functions
refer here.

|  |  |
|----|----|
| Term | Meaning |
| pdr_drawing | A named list for one drawing, with elements `.id`, `.file`, `.version`, `.width`, `.height`, `.units`, `.timestamp`, `.app`, `.strokes`, `.points`, and optionally `.rgba`, `.stencil` and `.stencil_inverted` |
| pdr_list | A list of pdr_drawings (typically the list-column `pdr_data`) |
| pdr_tibble | A tibble with columns `id` and a pdr_list column (default name `pdr_data`) |

Required elements of a pdr_drawing:

- `.id` (character): unique identifier

- `.width`, `.height` (integer): canvas dimensions

- `.points` (tibble): coordinates (required columns `.x`, `.y`,
  `.index`, all integer)

- `.strokes` (tibble): stroke values (required column `.index`, integer)

Optional: `.rgba` (numeric array with dimensions height x width x 4).

## Matching

With `action = "eval"` (default) and `as = ""`, the function returns a
string: `"pdr_tibble"`, `"pdr_list"`, `"pdr_drawing"` or `"invalid"`.
With `action = "eval"` and `as` set to one of the types, it returns
`TRUE` or `FALSE` for whether `input` is that type (an `input` that is
`NA` or `NULL` gives `FALSE`).

With `action = "coerce"`, `as` must be given and the function returns
`input` as that type. Supported conversions: pdr_list to pdr_tibble,
pdr_drawing to pdr_tibble, and pdr_drawing to pdr_list. If `input`
already is of the requested type it is returned unchanged; any other
conversion, or an `NA`/`NULL` input, is an error.

## Modes

With `mode = "superficial"` (default) a quick check of the structure of
`input` is made (a pdr_tibble has columns `id` and `pdr_data`; a
pdr_list is a list of lists; a pdr_drawing is a list with the required
elements). With `mode = "validate"`, a more detailed check of the
element types and of the contents of `.points`, `.strokes` and `.rgba`
is made (for a tibble, at least one list column must be a valid
pdr_list). Invalid `.rgba` gives a warning.

## Examples

``` r
pdr_example_data |> pdr_match() # "pdr_tibble"
#> [1] "pdr_tibble"

pdr_example_data |> pdr_match(as = "pdr_tibble") # TRUE
#> [1] TRUE

pdr_example_data |> pdr_match(as = "pdr_list") # FALSE
#> [1] FALSE

pdr_example_data$pdr_data |> pdr_match() # "pdr_list"
#> [1] "pdr_list"

pdr_example_data$pdr_data[[1]] |> pdr_match() # "pdr_drawing"
#> [1] "pdr_drawing"

# Coerce a pdr_list to a pdr_tibble with columns `id` and `pdr_data`
pdr_example_data$pdr_data |>
  pdr_match(action = "coerce", as = "pdr_tibble", mode = "validate")
#> # A tibble: 50 × 2
#>    id                               pdr_data         
#>    <chr>                            <list>           
#>  1 6d63804892233bbb18bc21bcf0d81d0d <named list [10]>
#>  2 c8b12cd45d08e94241d4de9990d60f71 <named list [10]>
#>  3 25062ac7dbeb08d8b532ac474a7849ca <named list [10]>
#>  4 0dc657fa6ed70150f9596fe692f2f9a0 <named list [10]>
#>  5 fb61c33ffa09270a5c7931813e9d2d71 <named list [10]>
#>  6 328d31503202db3e5c1b8e78f287810e <named list [10]>
#>  7 e07d8d7e7034ba3ae7f907de022af58d <named list [10]>
#>  8 9996f1a95c8e939c6ab6258bfc76f722 <named list [10]>
#>  9 41aa308e694ad270b7cfaea054c7c1a2 <named list [10]>
#> 10 0bcfd36a71b799e857f13c29011d21ff <named list [10]>
#> # ℹ 40 more rows
```
