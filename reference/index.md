# Package index

## Importing

Functions for importing data into paindrawr

- [`pdr_import_json()`](https://steenharsted.github.io/paindrawr/reference/pdr_import_json.md)
  : Import RedCap PainDraw JSON data
- [`pdr_import_png()`](https://steenharsted.github.io/paindrawr/reference/pdr_import_png.md)
  : Import PNG files as pdr drawings
- [`pdr_mird2pdr()`](https://steenharsted.github.io/paindrawr/reference/pdr_mird2pdr.md)
  : Convert MIRD pain drawings to a pdr_list

## Plotting

Functions for Plotting data.

- [`pdr_plot_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba.md)
  : Plot RGBA rasters in a grid
- [`pdr_plot_rgba_heatmap()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_rgba_heatmap.md)
  : Plot a pain heatmap from rendered RGBA rasters
- [`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)
  : Plot stencil data
- [`pdr_plot_xy()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_xy.md)
  : Recreate a pain drawing as a ggplot

## Image quantification

- [`pdr_summarize_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_summarize_rgba.md)
  : Summarise the alpha values of drawn pixels in a pain drawing

## paindrawr data modification

- [`pdr_add_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_rgba.md)
  : Add an RGBA raster array to a pain drawing based on spray
  coordinates
- [`pdr_explode()`](https://steenharsted.github.io/paindrawr/reference/pdr_explode.md)
  : Explode pain drawings into multiple drawings by stroke
- [`pdr_implode()`](https://steenharsted.github.io/paindrawr/reference/pdr_implode.md)
  : Implode multiple pain drawings into one
- [`pdr_modify_points()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_points.md)
  : Modify the points of pain drawings
- [`pdr_modify_rgba()`](https://steenharsted.github.io/paindrawr/reference/pdr_modify_rgba.md)
  : Modify the rgba data of pain drawings

## Stencils

Premade stencils to aid in image analysis

- [`pdr_add_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_add_stencil.md)
  : Create and add a stencil to pain drawings.
- [`pdr_plot_stencil()`](https://steenharsted.github.io/paindrawr/reference/pdr_plot_stencil.md)
  : Plot stencil data
- [`pdr_stencil_full_body`](https://steenharsted.github.io/paindrawr/reference/pdr_stencil_full_body.md)
  : Anatomical regions of the full body template, with stencils

## Example data

Example data and files

- [`pdr_example_data`](https://steenharsted.github.io/paindrawr/reference/pdr_example_data.md)
  : Example pain drawings with clinical variables
- [`pdr_example_file()`](https://steenharsted.github.io/paindrawr/reference/pdr_example_file.md)
  : Local absolute path to example files
- [`pdr_example_geometry`](https://steenharsted.github.io/paindrawr/reference/pdr_example_geometry.md)
  : Example pain drawings with simple geometric cases

## Helper functions

- [`pdr_match()`](https://steenharsted.github.io/paindrawr/reference/pdr_match.md)
  : Check or coerce pain drawing data types
- [`pdr_get_info()`](https://steenharsted.github.io/paindrawr/reference/pdr_get_info.md)
  : Extract information from pain drawing objects
