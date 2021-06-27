# Function to read and merge CT mfi statements from MS Access
extract_tables <- function(x) {
  
  #x <- "/Volumes/davidlucey/aDL/data/ct_muni_data/ct_mfi/ct_mfi_2003-07.mdb"
  
  # Get table names
  table_names <- Hmisc::mdb.get(x, tables = TRUE)
  
  # Select tables with mfi data based on regex
  table_names <-
    stringr::str_extract_all(table_names, "FISCIN\\d{2}", simplify = TRUE)[, 1]
  table_names <- table_names[table_names != ""]
  
  # Extract selected table_names
  table <-
    Hmisc::mdb.get(x,
                   tables = table_names,
                   allow = TRUE,
                   lowernames = TRUE)
  
  # Strip labels and create table names_list
  table <- lapply(table, Hmisc::cleanup.import)
  names_list <- lapply(table, names)
  names_list <-
    lapply(names_list, stringr::str_remove, pattern = "[[:digit:]]+")
  
  # Convert underlying add names to data.tables
  table <- lapply(table, as.data.table)
  table <- mapply(function(x, y)
    setnames(x, y), table, names_list)
  
  # Clean names
  table <- 
    lapply(table, janitor::clean_names, case = "snake")
  
}
