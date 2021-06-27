library(rsconnect)
library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(glue)

#Load data
yankee <- readRDS("data/yankee.RDS")

# Plot function
source("R/map_score.R")

ui <- fluidPage(
  titlePanel("Risk Score History of Selected Town"),
  br(),
  br(),
  selectInput(
    "Municipality", label = "Select Municipality:",
    choices = unique(yankee$Municipality), selected = "Bridgeport"),
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotly::plotlyOutput("spaghetti"), height="500px"),
                tabPanel("Table", tableOutput("table"))
    )
  )
)
server <- function(input, output) {
  
  output$spaghetti <- plotly::renderPlotly({
    
    map_score(yankee, input$Municipality)
    
  })
  
  output$table <- renderTable({
    
    yankee[Municipality == input$Municipality,.SD,.SDcols=patterns(c("Score|Year|Muni"))]
    
  })
  options = list(height = 1000)  
}

shinyApp(ui = ui, server = server)
