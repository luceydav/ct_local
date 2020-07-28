##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

get_mfi <- function() {
  
  #directory with data
  subdir <- "/Volumes/davidlucey/aDL/data/ct_muni_data/ct_mfi/"
  
  #Load 17 yrs MFI data
  ct_mfi <-
    do.call("c" , lapply(list.files(subdir, full.names = TRUE), extract_tables))
  ct_mfi <- lapply(ct_mfi, expss::drop_all_labels)
  ct_mfi <- lapply(ct_mfi, as.data.table)
  ct_mfi  <- rbindlist(ct_mfi,
                       fill = TRUE,
                       use.names = TRUE,
                       idcol = "ID")
  
}
