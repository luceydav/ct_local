# Build yankee dataset for 2009-2017 joining ct_mfi_pension, 
build_yankee <- function(data){
  
    # Load median real estate values from ct_real_assess project
    ct_re <- get_median_real_estate()
    
    #names(data) <- c("ct_mfi_DT", "ct_mfi_pension_DT", "ct_mfi_opeb_DT")
    #list2env(data, env = .GlobalEnv)
    data <- readRDS("data/ct_mfi_DT.RDS")
    ct_mfi_DT <- data[[1]]
    ct_mfi_pension_DT <- data[[2]]
    ct_mfi_opeb_DT <- data[[3]]
    rm(data)
    
    # ct_mfi_opeb, ct_re on `Fiscal Year` and `Municipality`
    yankee <-
      ct_mfi_pension_DT[ct_mfi_DT, on = c("Fiscal Year", "Municipality")]
    yankee <-
      ct_mfi_opeb_DT[yankee, on = c("Fiscal Year", "Municipality")]
    yankee <-
      ct_re[yankee, on = c("Fiscal Year", "Municipality")]
    
    # Order by `Fiscal Year` and setkeys
    yankee <- yankee[order(`Fiscal Year`)]
    setkeyv(yankee, c("Fiscal Year", "Municipality"))
    
    #Calc lag of unemployment and median home
    cols = c("Unempl.", "Median")
    lagcols = paste("lag", cols, sep = "_")
    yankee[, (lagcols) := shift(.SD, 1, 0, "lag"), 
           .SDcols = cols,
           by = "Municipality"]
    
    #Add A-E variables as calculated by Yankee Warning Signs for all years
    yankee[, `:=`(
      A = rowSums(yankee[, .(`Pension Cont. Req.`, `OPEB Cont. Req.`, `Debt Service`)], na.rm =
                    TRUE) / `Total Rev.`,
      B = `Total Funded Bal` / `Total Exp.`,
      C = rowSums(yankee[, .(`Total Bond. Debt`, `Net Pension Liab.`, `Net OPEB Liab.`)], na.rm = TRUE) / `Total Rev.`,
      D = `Unempl.` - lag_Unempl.,
      E = (`Median` - lag_Median) / lag_Median
    )]
    
    #Add score calc using calc_combo function by year in yankee list
    year <- as.character(c(2001:2018))
    yankee_list <- yankee[, list(list(.SD)), by = "Fiscal Year"]
    yankee <- lapply(yankee_list$V1, calc_combo)
    names(yankee) <- year
    yankee <-
      rbindlist(yankee, use.names = TRUE, idcol = "fisc_year_end")
    
    #Drop duplicates rows
    yankee <- unique(yankee)
    
    #Format variables for datatable and charts display
    yankee[, `:=`(
      `Fiscal Year` = fisc_year_end,
      `Municipality` = municipality,
      `Score` = score,
      `LTO Score` = lto_score,
      `Unempl. Score` = unemp_score,
      `Home Val. Score` = homeval_score,
      `Arc Score` = arc_score,
      `GF Score` = gf_score
    )]
    
    #Remove duplicate unformatted columns no longer needed
    yankee <-
      yankee[, .SD, .SDcols = !patterns("score|year|muni|max$|min$|wt$|slp$|^lag|Median|UnemplPopu")]
    
    #Summary of unique levels and NA's
    yankee[, lapply(.SD, uniqueN)][, melt(.SD)][order(value)]
    yankee[, lapply(.SD, function(x)(sum(is.na(x))) / .N)][
      ][, melt(.SD)][
        ][order(-value) & value > 0][
          ][, .(variable, format(value, scientific = FALSE))]
    
    #Clean up  
    rm(yankee_list)   
    
    return(yankee)
    
}