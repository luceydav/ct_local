#Function to get data from api for mfi and unemployment (empl)

get_api_data <- function() {
  
  mfi <-
    fread("https://data.ct.gov/resource/sb4i-6vik.csv")
  setnames(mfi,
           c("year", "town", "total_net_pos", "unrestricted_net_position", "unassigned_fund_balance", "change_in_net_position"),
           c("fisc_year_end", "municipality", "total_net_position", "unrestricted_net_assets", "unassigned_fund_bal", "change_in_net_assets"))
  
  empl <- 
    fread("https://data.ct.gov/resource/cugp-2za3.csv")
  empl <- 
    dcast(
      empl,
      benchmark_year + town ~ metric,
      value.var = "value",
      fun.aggregate = identity,
      fill = TRUE
    )
  drops <- 
    c("benchmark_year", "town", "Unemployed", "Labor Force")
  empl[, `:=`(
    fisc_year_end = benchmark_year,
    municipality = town,
    empl = Unemployed / `Labor Force`
  )][, (drops) := NULL]
    
  mfi <- 
    empl[mfi, on = c("fisc_year_end", "municipality")]
  
  return(mfi)
}