#Plot function
map_score <- function(dt, muni){
  
  frame <- dt[`Fiscal Year` %in% as.character(c(2004:2018)),
                  .SD,.SDcols=patterns("Score|Muni|Year")][
                  ][,melt(.SD, measure.vars=patterns("Score"))][
                  ][!is.na(value)]
  
  town <- frame[Municipality == muni]
  
  p <- ggplot(data=frame) +
    geom_line(aes(`Fiscal Year`,value,group=Municipality))+
    geom_line(data=town,
              aes(`Fiscal Year`,value,col="red",group=Municipality),size=0.5)+
    scale_color_manual(values = "red")+
    theme_bw() +
    theme(legend.position = "none")+
    labs(main="Breakdown of Score for {muni}",
         caption = "Source: State of CT OPM Municipal Fiscal Indicators") +
    xlab("Fiscal Year")+
    ylab("Risk Score")+
    scale_x_discrete(breaks=c(2005,2010,2015))+
    facet_wrap(~variable,scale="free",ncol=2)
  
  plotly::ggplotly(p)
  
}