pdr_put_info <- function(paindrawr_data, replacement, id1=".id", id2=NULL) {
  # paindrawr_data is expected the be a valid pain drawing
  # list-col
  # replacement_data is expected to a list of data to be put
  # inside paindrawr_data as a replacement for var1 element,
  # or as replacement for column var2 in element var1
  # The number of elements of replacement_data and the 
  # paindrawr_data element is replaces should match.

  if(is.null(col1)) {
    warning("No col1 parameter supplied")
    return(NA)
  }

  if(length(paindrawr_data) != length(replacement)) {
    warning("Mismatch in length of paindrawr_data (or sub-element tibble) and replacement")
    return(NA)
  }


  if(is.null(col2)) {
    if(col1 %in% c(".points", ".strokes", ".polygons")) {
      # stuff    
    } else {
      # stuff
    }
  } else {
    # stuff
  }
}