
clean_mfi <- function(dt) {


    #Replace fisc_year_end with idcol 
    dt[, fisc_year_end := paste0("20", stringr::str_extract(ID, "[[:digit:]]+"))]
    setkeyv(dt, c("fisc_year_end", "municipality"))
    
    #Unique rows to take out duplicate year
    dt <- 
      unique(dt, fromLast = TRUE, 
             by = c("fisc_year_end", "municipality"))
    
    # Function to remove "()" and convert to integer used for all segments
    convert_integer <- function(cell) {
      
      if( !is.na(cell) ) {
        
        cell <- 
          as.integer(str_replace(str_replace_all(cell, "[),]", ""), "\\(", "-"))
        
      } else {
        
        cell <- NA_integer_
      }
      return(cell)
    }
    dt[, unrestricted_net_assets :=
         sapply(unrestricted_net_assets, convert_integer)]
    
    drops <-
      c(
        "x_population",
        "x_nonspendable_fund_bal",
        "x_restricted_fund_bal",
        "x_committed_fund_bal",
        "x_assigned_fund_bal",
        "unrestricted_net_assets",
        "unrestricted_fund_bal",
        "curr_year_tax_collection",
        "overal_total_tax_coll_rate",
        "tot_bnd_lng_tr_det_rsd_town",
        "net_invstmnt_in_capital_assets",
        "invested_in_capital_assets_net_of_related_debt"
      )
    dt[, `:=`(
      population = fcoalesce(x_population, population),
      nonspendable_fund_bal = fcoalesce(x_nonspendable_fund_bal, nonspendable_fund_bal),
      restricted_fund_bal = fcoalesce(x_restricted_fund_bal, as.integer(restricted_fund_bal)),
      committed_fund_bal = fcoalesce(x_committed_fund_bal, as.integer(committed_fund_bal)),
      assigned_fund_bal = fcoalesce(x_assigned_fund_bal, as.integer(assigned_fund_bal)),
      unrestricted_net_position = 
        fcoalesce(unrestricted_net_assets,
                  unrestricted_net_position,
                  unrestricted_fund_bal),
      curr_year_tax_collection_rate = fcoalesce(
        curr_year_tax_collection_rate,
        curr_year_tax_collection
      ),
      overal_total_tax_collection_rate = fcoalesce(
        overal_total_tax_collection_rate,
        overal_total_tax_coll_rate
      ),
      total_bonded_long_term_debt_rsd_town = fcoalesce(
        as.integer(total_bonded_long_term_debt_rsd_town),
        tot_bnd_lng_tr_det_rsd_town
      ), 
      net_investment_in_capital_assets = fcoalesce(
        net_investment_in_capital_assets,
        invested_in_capital_assets_net_of_related_debt,
        net_invstmnt_in_capital_assets
      )
      )][, (drops) := NULL]
    
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
    
    # unrestricted_net_assets <- 
    #   names(dt)[stringr::str_detect(names(dt),"unrestricted" ) & stringr::str_detect(names(dt), "net")]
    # dt <-
    #   dt[, unrestricted_net_assets := apply(.SD, 1, function(x)
    #     na.omit(x)[1]), .SDcols = unrestricted_net_assets, 
    #     by = c("municipality", "fisc_year_end")][
    #     ][, .SD, .SDcols = !names(dt) %in% unrestricted_net_assets]
    
    tanf <- 
      names(dt)[stringr::str_detect(names(dt), "tfa")
                          | stringr::str_detect(names(dt), "tanf")]
    dt <-
      dt[, tanf := apply(.SD, 1, function(x)
        na.omit(x)[1]),
        .SDcols = tanf, by = c("municipality", "fisc_year_end")][
        ][, .SD, .SDcols = !names(dt) %in% tanf]
    
    # patterns <- c("capital", "rsd")
    # for (pattern in patterns) {
    #   vars <- names(dt)[stringr::str_detect(names(dt), pattern)]
    #   dt[, (pattern) := apply(.SD, 1, function(x)
    #     na.omit(x)[1]), .SDcols = vars, by = c("municipality", "fisc_year_end")][
    #     ][, .SD, .SDcols = !names(dt) %in% vars]
    # }
    
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
                 ID=NULL)]
    # dt[,.SD,.SDcols=!patterns("fund_bal")]
    
    vars <-
      c(
        "fisc_year_end",
        "municipality",
        "total_revenue",
        "total_expenditures",
        "debt_service",
        "total_bonded_long_term_debt_rsd_town",
        "total_fund_bal",
        "unrestricted_net_position",
        "empl",
        "total_net_assets",
        "change_in_net_assets",
        "total_net_pension_liability",
        "population",
        "net_investment_in_capital_assets"
      )
    dt <- dt[, ..vars]
    
    dt_DT  <-
      dt[, .(
        `Fiscal Year` = fisc_year_end,
        `Municipality` = municipality,
        `Total Rev.` = total_revenue,
        `Total Exp.` = total_expenditures,
        `Debt Service` = debt_service,
        `Total Bond. Debt` = total_bonded_long_term_debt_rsd_town,
        `Total Funded Bal` = total_fund_bal,
        `Unrestricted` = unrestricted_net_position,
        `Unempl.` = empl,
        `Total Net Asset` = total_net_assets,
        `Chg. Net Asset` = change_in_net_assets,
        `Net Pension Liab.` = total_net_pension_liability,
        `Popu.` = population,
        `Capital Inv.` = net_investment_in_capital_assets
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