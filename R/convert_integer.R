
# Function to remove "()" and convert to integer used for all segments
convert_integer <- function(cell) {
  
  if( !is.na(cell) ) {
    
    cell <- 
      as.integer(str_replace(str_replace_all(cell, "[),]", ""), "\\(", "-"))
    
  } else {
    
    cell <- NA_integer_
  }
  return(cell)
}