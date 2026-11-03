# paindrawr: Quantification of pain drawings

Provides tools for importing, processing, quantifying, and visualising
digital pain drawings. Pain drawings are commonly used in clinical
research to document the location and distribution of pain on a
schematic representation of the human body. Digital pain drawings can
capture individual pen strokes as x- and y-coordinates, allowing more
detailed analyses than traditional region-based classifications. The
package supports the reconstruction of individual drawings,
quantification of marked areas, anatomical region-based analyses, and
the generation of group-level pain heatmaps. It is designed to work with
data collected using the 'paindraw' REDCap external module, but also
supports data from other sources.

## Terminology

|  |  |
|----|----|
| Term | Meaning |
| pdr_drawing | Named list for one drawing: `.id`, `.file`, `.version`, `.width`, `.height`, `.units`, `.timestamp`, `.app`, `.strokes`, `.points`, optional `.rgba`, `.stencil`, `.stencil_inverted` |
| pdr_list | A list of pdr_drawings (typically the list-column `pdr_data`) |
| pdr_tibble | A tibble with columns `id` and a pdr_list column (default name `pdr_data`) |

See
[`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
for validating and coercing between these types.

## See also

Useful links:

- <https://github.com/steenharsted/paindrawr>

- <https://steenharsted.github.io/paindrawr/>

- Report bugs at <https://github.com/steenharsted/paindrawr/issues>

## Author

**Maintainer**: Søren O'Neill <soren@oneill.dk>

Authors:

- Søren O'Neill <soren@oneill.dk>

- Steen Harsted <sharsted@mmmi.sdu.dk>
