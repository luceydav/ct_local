<<<<<<< HEAD

## Analysis of State of CT open mfi, pension and opeb data

This Project downloads all [State of Connecticut OPB Municipal Fiscal Indicators (MFI) reports](https://portal.ct.gov/OPM/IGP-MUNFINSR/Municipal-Financial-Services/Municipal-Fiscal-Indicators)
from open public Microsoft Access databases. This data is used in the blogpost: 
[Connecticut City Unfunded Pension and OPEB Liabilities Over Time](https://redwallanalytics.com/2019/10/11/connecticut-city-unfunded-pension-and-opeb-liabilities-over-time/)

A drake workflow extracts three tables, key metrics from the annual annual CAFRs, pensions and opeb tables of the Municipal Fiscal Indictors databases, and 
aggregates and saves "data/ct_mfi_DT.RDS" for the years 2001-2018.

The drake workflow then attempts to replicate the risk metrics from The Yankee Intitute 
[Warnings 
Signs: Assessing Municipal Fiscal Health in Connecticut](https://yankeeinstitute.org/wp-content/uploads/2018/08/Warning-Signs-min-1.pdf) 
report, but for year's 2004-2018 (instead of just 2016) and saves it in "data/yankee.RDS". 

Because of differences in the available data, the
risk scores will be similar to what would have been calculated by the Yankee Institute. The output yankee.csv is
used in the Shiny app (yankee_shiny). See the blogpost for further detail: 
[Replicating Yankee Institute Risk Score over 15 Years](https://redwallanalytics.com/2019/10/12/replicating-yankee-institute-risk-score-over-15-years/)

# Shiny App
The yankee_shiny folder contains a Shiny app used in Replicating Yankee Institute blogpost.

=======
# ct_local
State of CT open mfi, pension and opeb ETL, and Shiny app of municipal risk over time
>>>>>>> 36cc3dc9c207863dec25aa578d84d86a6f6be79e
