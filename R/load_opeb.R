
load_opeb <- function(){
  
  #directory with data
  subdir <- "/Volumes/davidlucey/aDL/data/ct_muni_data/ct_mfi/"
  
  # Extract opeb data from ct_mfi file and build data.table
  ct_mfi_opeb <-
    do.call("c" , lapply(
      list.files(subdir, pattern = "mdb", full.names = TRUE),
      extract_opeb
    ))
  names <-
    stringr::str_extract(names(ct_mfi_opeb), "\\d{4}")
  
  # Add current year
  socrata_opeb <- 
    fread("https://data.ct.gov/resource/sa26-46h8.csv")
  names(ct_mfi_opeb) <- names
  ct_mfi_opeb[["2019"]] <- socrata_opeb
  
  #Add all years back together in data.table names by year
  ct_mfi_opeb <-
    rbindlist(ct_mfi_opeb,
              use.names = TRUE,
              fill = TRUE,
              idcol = "fisc_year_end")
  ct_mfi_opeb <-
    unique(ct_mfi_opeb, by = c("fisc_year_end", "entity", "plan_name"))
  ct_mfi_opeb[, municipality := str_to_title(fcoalesce(town, entity))]
  
  # Clean up, transform and select vars to keep based on pattern
  ct_mfi_opeb <-
    unique(ct_mfi_opeb,
           by = c("fisc_year_end", "municipality", "plan_name"))
  
  # pattern <-
  #   "emplyr_contrib|aal|asset_value|year|municipality|members|contrib|adec"
  # ct_mfi_opeb <-
  #   ct_mfi_opeb[, .SD, .SDcol = patterns(pattern)]
  
  # Convert numeric
  cols <-
    c("no_of_members",
      "req_d_emplyr_contrib",
      "emplyr_contrib_made",
      "emplyr_contrib_made_gasb75",
      "gasb45_aal",
      "gasb45_asset_value",
      "number_of_members",
      "contrib_percentage",
      "adec",
      "employer_contribution",
      "plan_fiduciary_net_positon",
      "total_opeb_liability",
      "adec_gasb75",
      "gasb75_net_position",
      "gasb75_topebl" 
    )
  ct_mfi_opeb[, (cols) := lapply(.SD, as.integer), .SDcols = cols]
  
  # Coalesce vars with different names in different years
  ct_mfi_opeb[, `:=`(
    req_emplyr_contrib = fcoalesce(
      req_d_emplyr_contrib,
      adec,
      adec_gasb75  
    ),
    emplyr_contrib_made = fcoalesce(
      emplyr_contrib_made,
      employer_contribution,
      emplyr_contrib_made_gasb75
    ),
    actuarial_liability = fcoalesce(
      gasb45_aal,
      total_opeb_liability,
      gasb75_topebl
    ),
    number_of_members = fcoalesce(
      no_of_members,
      number_of_members
    ),
    asset_value = fcoalesce(
      gasb45_asset_value,
      plan_fiduciary_net_positon,
      gasb75_net_position  
    )
  )]
  
  # Aggregate columns by year/municipality
  cols <-
    c(
      "req_emplyr_contrib",
      "emplyr_contrib_made",
      "actuarial_liability",
      "asset_value",
      "number_of_members"
    )
  opeb_data <-
    ct_mfi_opeb[, 
      lapply(.SD, sum, na.rm = TRUE), 
      .SDcol = cols, 
      .(fisc_year_end, municipality)][
        ][order(fisc_year_end)]
  
  # Build dataframe names with tickmarks for display in DT datatable
  contrib <-
    ct_mfi_opeb[, 
      .(contrib_percentage = mean(contrib_percentage, na.rm = TRUE)), 
      .(fisc_year_end, municipality)][
      ][order(fisc_year_end)]
  ct_mfi_opeb_DT <- 
    data.table(opeb_data, contrib)
  
  # Format names
  ct_mfi_opeb_DT  <-
    ct_mfi_opeb_DT[, .(
      `Fiscal Year` = fisc_year_end,
      `Municipality` = municipality,
      `OPEB Covered Part.` = number_of_members,
      `Emplyee. OPEB Cont.` = fifelse(!is.nan(contrib_percentage), contrib_percentage * 100, 0),
      `OPEB Cont. Req.` = req_emplyr_contrib,
      `OPEB Cont. Made.` = emplyr_contrib_made,
      `OPEB Cont. Percent.` =
        ifelse(
          !(is.nan(emplyr_contrib_made / req_emplyr_contrib) | 
            is.infinite(emplyr_contrib_made / req_emplyr_contrib)),
          emplyr_contrib_made / req_emplyr_contrib,
          0),
      `OPEB Liab.` = actuarial_liability,
      `OPEB Net Asset` = asset_value,
      `Net OPEB Liab.` = actuarial_liability - asset_value,
      `OPEB Percent Funded` = asset_value / actuarial_liability
    )]
  
  #Clean up
  rm(contrib,opeb_data)
  
  return(ct_mfi_opeb_DT)
  
}