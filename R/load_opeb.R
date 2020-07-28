
load_opeb <- function(){
  
  #directory with data
  subdir <- "/Volumes/davidlucey/aDL/data/ct_muni_data/ct_mfi/"
  
  # Extract opeb data from ct_mfi file and build data.table
  ct_mfi_opeb <-
    do.call("c" , lapply(list.files(subdir, full.names = TRUE), extract_opeb))
  names(ct_mfi_opeb) <-
    stringr::str_extract(names(ct_mfi_opeb), "\\d{4}")
  ct_mfi_opeb <-
    rbindlist(ct_mfi_opeb,
              use.names = TRUE,
              fill = TRUE,
              idcol = "year")
  
  # Clean up, transform and select vars to keep based on pattern
  ct_mfi_opeb <- unique(ct_mfi_opeb, by = c("year", "entity", "plan_name"))
  setnames(ct_mfi_opeb,
           c("year", "entity"),
           c("fisc_year_end", "municipality"))
  
  pattern <-
    "emplyr_contrib|aal|asset_value|year|municipality|members|contrib"
  ct_mfi_opeb <-
    ct_mfi_opeb[, .SD, .SDcol = patterns(pattern)][
    ][, municipality := str_to_title(municipality)]
  
  # Convert numeric
  cols <-
    c(
      "req_d_emplyr_contrib",
      "emplyr_contrib_made",
      "gasb45_aal",
      "gasb45_asset_value",
      "number_of_members",
      "contrib_percentage"
    )
  ct_mfi_opeb[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  
  # Aggregate columns by year/municipality
  cols <-
    c(
      "req_d_emplyr_contrib",
      "emplyr_contrib_made",
      "gasb45_aal",
      "gasb45_asset_value",
      "number_of_members"
    )
  opeb_data <-
    ct_mfi_opeb[, lapply(.SD, sum, na.rm = TRUE), .SDcol = cols, .(fisc_year_end, municipality)][order(fisc_year_end)]
  
  # Build dataframe names with tickmarks for display in DT datatable
  contrib <-
    ct_mfi_opeb[, mean(contrib_percentage, na.rm = TRUE), 
                .(fisc_year_end, municipality)][
                ][order(fisc_year_end)]
  ct_mfi_opeb_DT <- data.table(opeb_data, contrib)
  ct_mfi_opeb_DT  <-
    ct_mfi_opeb_DT[, .(
      `Fiscal Year` = fisc_year_end,
      `Municipality` = municipality,
      `OPEB Covered Part.` = number_of_members,
      `Emplyee. OPEB Cont.` = V1 * 100,
      `OPEB Cont. Req.` = req_d_emplyr_contrib,
      `OPEB Cont. Made.` = emplyr_contrib_made,
      `OPEB Cont. Percent.` =
        emplyr_contrib_made / req_d_emplyr_contrib,
      `OPEB Liab.` = gasb45_aal,
      `OPEB Net Asset` = gasb45_asset_value,
      `Net OPEB Liab.` = gasb45_aal - gasb45_asset_value,
      `OPEB Percent Funded` = gasb45_asset_value / gasb45_aal
    )]
  
  #Clean up
  rm(contrib,opeb_data)
  
  return(ct_mfi_opeb_DT)
  
}