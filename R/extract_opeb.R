
# Function to extract tables with OPEB data from CT Microsoft Access databases
extract_opeb <- function(x) {
  
  # Get table names
  tables <-
    Hmisc::mdb.get(x,
                   tables = TRUE,
                   allow = TRUE,
                   lowernames = TRUE)
  
  # Separate tables with OPEB data
  opeb_tables <-
    tables[stringr::str_detect(tables, "OPEB|opeb|Opeb")]
  # Further separate tables with "Data" in name
  opeb_tables <-
    opeb_tables[stringr::str_detect(opeb_tables, "Data")]
  
  # Get "OPEB Data" tables from database and clean labels 
  if (length(opeb_tables) > 0) {
    table <- Hmisc::mdb.get(x, tables = opeb_tables)
  } else {
    table <- list()
  }
  if (inherits(table, "list") & length(table) > 0) {
    table <- lapply(table, as.matrix)
  } else {
    table <- as.matrix(table)
  }
  if (inherits(table, "list") & length(table) > 0) {
    table <- lapply(table, as.data.table)
  } else {
    table <- as.data.table(table)
  }
  if (!inherits(table, "list")) {
    table <- list(table)
    names(table) <- opeb_tables
  }
  
  # Drop faulty tables which don't have appropriate rows/cols
  table <- table[unlist(lapply(table,ncol))>10]
  table <- table[unlist(lapply(table,nrow))>100]
  
  # Clean names
  table <- lapply(table, janitor::clean_names, case="snake")
  
  return(table)
}