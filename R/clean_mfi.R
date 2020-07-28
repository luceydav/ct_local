
clean_mfi <- function(dt) {


    #Replace fisc_year_end with idcol 
    dt[, fisc_year_end := paste0("20", stringr::str_extract(ID, "[[:digit:]]+"))]
    
    #Unique rows to take out duplicate year
    dt <- unique(dt, by = c("fisc_year_end", "municipality"))
    
    # Clean duplicate variables in mfi
    change_in_net_assets <-
      names(dt)[stringr::str_detect(names(dt), "change") &
                      stringr::str_detect(names(dt), "net")]
    dt <-
      dt[, change_in_net_assets := apply(.SD, 1, function(x)
        na.omit(x)[1]),
        .SDcols = change_in_net_assets, 
        by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% change_in_net_assets[2]]
    
    total_net_assets <-
      names(dt)[stringr::str_detect(names(dt), "total") &
                      stringr::str_detect(names(dt), "net") &
                      stringr::str_detect(names(dt), "assets|position")]
    dt <-
      dt[, total_net_assets := apply(.SD, 1, function(x)
        na.omit(x)[1]), .SDcols = total_net_assets, 
        by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% total_net_assets[2]]
    
    unrestricted_net_assets <- names(dt)[stringr::str_detect(names(dt),"unrestricted" )& stringr::str_detect(names(dt), "net")]
    dt <-
      dt[, unrestricted_net_assets := apply(.SD, 1, function(x)
        na.omit(x)[1]), .SDcols = unrestricted_net_assets, 
        by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% unrestricted_net_assets]
    
    tanf <- names(dt)[stringr::str_detect(names(dt), "tfa")
                          | stringr::str_detect(names(dt), "tanf")]
    dt <-
      dt[, tanf := apply(.SD, 1, function(x)
        na.omit(x)[1]),
        .SDcols = tanf, by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% tanf]
    
    patterns <- c("capital", "rsd", "unrestricted")
    for (pattern in patterns) {
      vars <- names(dt)[stringr::str_detect(names(dt), pattern)]
      dt[, (pattern) := apply(.SD, 1, function(x)
        na.omit(x)[1]), .SDcols = vars, by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% vars]
    }
    
    #Change Moody vars to chr and coalesc
    factors <- c(
      "moody_s_bond_ratings_july",
      "moody_s_bond_ratings_june",
      "moody_s_bond_ratings_nov",
      "moody_s_bond_ratings_latest",
      "date_of_latest_moody_s_ratings",
      "moody_s_bond_ratings_dec",
      "acmr"
    )
    dt[, (factors) := lapply(.SD, function(x)
      levels(x)[as.numeric(x)]), .SDcols = factors]
    moody <-
      names(dt)[stringr::str_detect(names(dt), "moody") &
                      !stringr::str_detect(names(dt), "date")]
    dt <-
      dt[, moody := apply(.SD, 1, function(x)
        na.omit(x)[1]), .SDcols = moody, 
        by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% moody]
    dt[,moody := ifelse(moody=="","None",moody)]
    #ct_mfi$moodys <- ordered(ct_mfi$moodys_ratings, levels=c("None","Aaa", "Aa1",  "Aa2",  "Aa3",  "AA3", "A1", "A2", "A3","Baa1", "Baa2", "Baa3","Ba2"))
    
    dt[,`:=`(municipality=stringr::str_to_title(municipality),
                 ID=NULL)][,.SD,.SDcols=!patterns("fund_bal")]
    
    vars <-
      c(
        "fisc_year_end",
        "municipality",
        "total_revenue",
        "total_expenditures",
        "debt_service",
        "total_bonded_long_term_debt_rsd_town",
        "total_fund_bal",
        "unrestricted",
        "empl",
        "total_net_assets",
        "change_in_net_assets",
        "total_net_pension_liability",
        "population",
        "capital"
      )
    dt <- dt[, population := x_population][, ..vars]
    
    dt_DT  <-
      dt[, .(
        `Fiscal Year` = fisc_year_end,
        `Municipality` = municipality,
        `Total Rev.` = total_revenue,
        `Total Exp.` = total_expenditures,
        `Debt Service` = debt_service,
        `Total Bond. Debt` = total_bonded_long_term_debt_rsd_town,
        `Total Funded Bal` = total_fund_bal,
        `Unrestricted` = unrestricted,
        `Unempl.` = empl,
        `Total Net Asset` = total_net_assets,
        `Chg. Net Asset` = change_in_net_assets,
        `Net Pension Liab.` = total_net_pension_liability,
        `Popu.` = population,
        `Capital Inv.` = capital
      )]
    
    rm(
      change_in_net_assets,
      total_net_assets,
      unrestricted_net_assets,
      factors,
      moody,
      pattern,
      patterns,
      subdir,
      tanf
    )   
    
    return(dt_DT)
}