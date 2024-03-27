library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
source("../make_funnelplot.R")
source("../check_demographic_parity.R")

#have to separate data between before and after intervention on 3/1/2028

uds_dat <- read.csv("../uds_dat.csv")
colnames(uds_dat) <- gsub("\\.", " ", colnames(uds_dat))

ui <- fluidPage(h1("Equity Calculator"), br(), br(),
  tabsetPanel(
    tabPanel("Urine Drug Screening Example",
             DTOutput("tbl"),
             plotlyOutput("plot"),
             DTOutput("dempar")),
    tabPanel("Your Data",
             fileInput("dat", "Upload your data"),
             DTOutput("tbl2"),
             plotlyOutput("plot2"),
             DTOutput("dempar2"))
    )
)

server <- function(input, output) {
  
  output$tbl <- renderDT({
    datatable(uds_dat)
  })
  
  output$tbl2 <- renderDT({
    req(input$dat)
    datatable(input$dat)
  })
  
  output$plot <- renderPlotly(make_funnelplot(uds_dat))
  
  output$plot2 <- renderPlotly({
    req(input$dat)
    make_funnelplot(input$dat)
    })
  
  output$dempar <- renderDT({
    datatable(check_demographic_parity(uds_dat))
  })
  
  output$dempar2 <- renderDT({
    req(input$dat)
    datatable(check_demographic_parity(input$dat))
  })
  
}

shinyApp(ui = ui, server = server)
