# load real estate sales data from ct_real_assess and calc median selling price by town/year
get_median_real_estate <- function() {
  
    path <- "/Users/davidlucey/Desktop/David/Projects/ct_real_assess/"

    # Load all CT Housing sales since 2001 for calculation and addition of median values
    ct_re <- 
      setDT(readRDS(paste0(path, "ct_sales_99_2018.RDS")))
    
    # Filter for single family and select variables
    ct_re <- 
      ct_re[property_type == "Single Family"][
            ][(non_use_code == "0" | is.na(non_use_code))]
    
    # Replace some missing SalesAmount when SalesRatio is available
    #ct_re[is.na(SaleAmount), SaleAmount := AssessedValue / SalesRatio]
    
    #Change year to chr
    ct_re[, year := as.character(year)]
    
    # Change names to Formatting for display in table
    setnames(ct_re,
             c("town", "year"),
             c("Municipality", "Fiscal Year"))
    
    # Calc median value by muni and year
    ct_re <-
      ct_re[, .(
        Median = median(sale_price, na.rm = TRUE)
      ),
      .(`Municipality`, `Fiscal Year`)]
    
    # If missing 2018, add 2017
    munis <- unique(ct_re$Municipality)
    missing_munis <- 
      setdiff(munis, unique(ct_re[`Fiscal Year` == "2018", Municipality]))
    missing_year <-
      ct_re[Municipality %in% missing_munis & 
              `Fiscal Year` == "2017"]
    missing_year$`Fiscal Year` <- "2018"
    ct_re <- rbind(ct_re, missing_year)
    
    return(ct_re)
}
    