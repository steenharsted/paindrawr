# drawings missing .stencil error

    Code
      pdr_implode_stencils(list(no_stencil, b))
    Condition
      Error in `pdr_implode_stencils()`:
      ! All drawings in `pdr_list` must have a .stencil element. See `pdr_add_stencil()`.

# drawings with different canvas dimensions error

    Code
      pdr_implode_stencils(list(a, wide))
    Condition
      Error in `pdr_implode_stencils()`:
      ! All drawings must have identical .width and .height.

# stencil matrices with wrong dimensions error

    Code
      pdr_implode_stencils(list(a, wrong))
    Condition
      Error in `pdr_implode_stencils()`:
      ! All .stencil matrices must have dimensions height x width (2 x 3).

# drawings with different units error

    Code
      pdr_implode_stencils(list(a, inches))
    Condition
      Error in `pdr_implode_stencils()`:
      ! All drawings must have the same .units.

