##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

get_mfi <- function() {
  
  #directory with data
  subdir <- "/Volumes/davidlucey/aDL/data/ct_muni_data/ct_mfi"
  
  #Load 17 yrs MFI data
  ct_mfi <-
    do.call("c" , lapply(list.files(subdir, pattern = "mdb", full.names = TRUE), extract_tables))
  ct_mfi <- lapply(ct_mfi, expss::drop_all_labels)
  ct_mfi <- lapply(ct_mfi, as.data.table)
  # ct_mfi <- ct_mfi[which(!duplicated(names(ct_mfi), fromLast = TRUE))]
   socrata <- get_api_data()
  # ct_mfi <- 
  #   lapply(ct_mfi, function(muni) { 
  #     names <- names(muni)
  #     names <- str_remove_all(names, "^x\\_")
  #     names(muni) <- names
  #     return(muni)
  # })
  ct_mfi  <- rbindlist(
    ct_mfi,
    fill = TRUE,
    use.names = TRUE,
    idcol = "ID")
  
  socrata$ID <- "FISCIN19"
  ct_mfi <- rbind(ct_mfi, socrata, fill = TRUE)
  
  return(ct_mfi)

}
