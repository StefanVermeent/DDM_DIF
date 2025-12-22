extract_attrs <- function(data, attr_type){
  purrr::map_chr(data, function(x){
    
    # For variable labels
    if(attr_type == "label"){
      extracted_attr <- attr(x, which = attr_type)
      # If it's null, make it an empty string
      if (is.null(extracted_attr)) {
        extracted_attr <- ""
      }
      extracted_attr
    }
    
    # For variable values
    if(attr_type == "labels"){
      extracted_attr <- attr(x, which = attr_type)
      if (is.null(extracted_attr)) {
        extracted_attr <- ""
      }
      else {
        extracted_attr <- paste(names(extracted_attr),"=", extracted_attr, collapse = ", ")
      }
      extracted_attr
    }
    
    extracted_attr
  })
}

create_codebook <- function (data) {
  # create a lookup table for a labelled dataset
  lookup_data <- 
    dplyr::tibble(
      Variable = names(data), 
      Label    = extract_attrs(data, "label"), 
      Values   = extract_attrs(data, "labels") 
    )
  
  codebook <- lookup_data
  no_labels <- codebook %>% dplyr::filter(Label == "")
  no_values <- codebook %>% dplyr::filter(Values == "")
  if (nrow(no_labels) > 0) {
    message("---------The following variables have no labels:---------\n", 
            paste(no_labels %>% dplyr::pull(Variable), collapse = "\n"))
  }
  if (nrow(no_values) > 0) {
    message("---------The following variables have no value labels:---------\n", 
            paste(no_values %>% dplyr::pull(Variable), collapse = "\n"))
  }
  codebook
      
  
}
