pdr_put_info <- function(pdr_list, replacement, id1=".id", id2=NULL) {
  # pdr_list is expected the be a valid pain drawing
  # list-col
  # replacement_data is expected to a list of data to be put
  # inside pdr_list as a replacement for var1 element,
  # or as replacement for column var2 in element var1
  # The number of elements of replacement_data and the 
  # pdr_list element is replaces should match.

  if(is.null(id1)) {
    warning("No 'id1' parameter supplied")
    return(NA)
  }

  if(length(pdr_list) != length(replacement)) {
    warning("Mismatch in length of pdr_list (or sub-element tibble) and replacement")
    return(NA)
  }


  if(is.null(col2)) {
    if(id1 %in% c(".points", ".strokes", ".polygons")) {
      # stuff    
    } else {
      # stuff
    }
  } else {
    # stuff
  }
}