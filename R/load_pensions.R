
load_pension <- function(){
  
  #directory with data
  subdir <- "/Volumes/davidlucey/aDL/data/ct_muni_data/ct_mfi/"
  
  #Extract pension data from ct_mfi file
  ct_mfi_pension <-
    do.call("c" , lapply(list.files(subdir, full.names = TRUE), extract_pension))
  names <- names(ct_mfi_pension)
  
  #Eliminate labels by converting to matrix then data.table
  ct_mfi_pension <- lapply(ct_mfi_pension, as.matrix)
  ct_mfi_pension <- lapply(ct_mfi_pension, as.data.table)
  names(ct_mfi_pension) <- names
  
  #Add all years back together in data.table names by year
  ct_mfi_pension <-
    rbindlist(ct_mfi_pension,
              use.names = TRUE,
              fill = TRUE,
              idcol = "year")
  ct_mfi_pension <-
    unique(ct_mfi_pension, by = c("year", "entity", "plan_name"))
  setnames(ct_mfi_pension,
           c("entity", "year"),
           c("municipality", "fisc_year_end"))
  
  #Coalesce variables which have different names in different annual tables
  ct_mfi_pension <-
    ct_mfi_pension[, `:=`
                   (
                     emplyr_cont_req =
                       as.numeric(
                         fcoalesce(req_d_emplyr_contrib,
                                   emplyr_required_adc_gasb67)
                       ),
                     emplyr_cont_made =
                       as.numeric(
                         fcoalesce(emplyr_contrib_made,
                                   emplyr_contribution_gasb67)
                       ),
                     pension_net_ass =
                       as.numeric(
                         fcoalesce(
                           gasb5_net_assets,
                           gasb27_asset_value,
                           plan_fiduciary_net_position_gasb6768
                         )
                       ),
                     pension_liab = as.numeric(
                       fcoalesce(gasb27_aal,
                                 total_pension_liability_gasb6768)
                     ),
                     number_of_members = as.numeric(number_of_members),
                     inv_returns = as.numeric(investment_rate_of_return_gasb6768),
                     municipality = str_to_title(municipality)
                   )]
  
  #Select cols to keep
  cols <- c(
    "fisc_year_end",
    "municipality",
    "number_of_members",
    "emplyr_cont_req",
    "emplyr_cont_made",
    "pension_liab",
    "pension_net_ass",
    "inv_returns"
  )
  ct_mfi_pension <- ct_mfi_pension[, ..cols]
  
  
  #Aggregate numeric by town/year
  cols <- c(
    "emplyr_cont_req",
    "emplyr_cont_made",
    "number_of_members",
    "pension_liab",
    "pension_net_ass"
  )
  pension_data <-
    ct_mfi_pension[, lapply(.SD, sum, na.rm = TRUE),
                   .SDcols = cols,
                   .(fisc_year_end, municipality)]
  
  #Aggregate mean by town/year
  inv_returns <-
    ct_mfi_pension[, (mean(inv_returns, na.rm = TRUE) * 100),
                   .(fisc_year_end, municipality)]
  
  #Rebuild with town/year aggregations
  ct_mfi_pension <- data.table(pension_data,
                               inv_returns = inv_returns$V1)
  
  ct_mfi_pension_DT <-
    ct_mfi_pension[, .(
      `Fiscal Year` = fisc_year_end,
      `Municipality` = municipality,
      `Covered Part.` = number_of_members,
      `Pension Cont. Req.` = emplyr_cont_req,
      `Pension Cont. Made.` = emplyr_cont_made,
      `Pension Cont. Percent` = emplyr_cont_made / emplyr_cont_req,
      `Pension Liab.` = pension_liab,
      `Pension Net Asset` = pension_net_ass,
      `Pension Return Ass.` = inv_returns,
      `Net Pension Liab.` = pension_liab - pension_net_ass,
      `Percent Funded` = pension_net_ass / pension_liab
    )]
  
  #Cleanup
  rm(inv_returns,pension_data)
  
  return(ct_mfi_pension_DT)
  
}