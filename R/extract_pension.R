extract_pension <- function(x){
  
  # Get table names from Microsoft Access db
  tables <-
    Hmisc::mdb.get(x,
                   tables = TRUE,
                   allow = TRUE,
                   lowernames = TRUE)
  # Filter names with word "Pension"
  pension_tables <- tables[stringr::str_detect(tables, "Pension")]
  # Futher filter for word "Data"
  pension_tables <-
    pension_tables[stringr::str_detect(pension_tables, "Data")]
  # Name based on year extracted regex
  table_names <-
    stringr::str_extract_all(pension_tables, "\\d{4}", simplify = TRUE)[, 1]
  # Get pension tables from Microsoft access
  table <- Hmisc::mdb.get(x, tables = pension_tables)
  
  # Strip labels from variables
  if (inherits(table, "list")) {
    table <- lapply(table, as.matrix)
  } else {
    table <- table
  }
  if (inherits(table, "list")) {
    table <- lapply(table, as.data.table)
  } else {
    table <- table
  }
  if (!inherits(table, "list"))
    table <- list(table)
  # Name tables and clean underlying data.table names
  names(table) <- table_names
  table <- lapply(table, janitor::clean_names, case = "snake")
  
  return(table)
}