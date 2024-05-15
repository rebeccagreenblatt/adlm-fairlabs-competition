library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
source("../make_funnelplot.R")
source("../check_demographic_parity.R")

uds_dat <- read.csv("../uds_dat.csv")
colnames(uds_dat) <- gsub("\\.", " ", colnames(uds_dat))

ui <- fluidPage(h1("Equity Calculator"), br(), br(),
  tabsetPanel(
    tabPanel("Maternal Urine Drug Screening Example",
             p("This data was provided as part of the 2024 ADLM FairLabs Data Analytics Challenge"),
             a("More info here", href = "https://www.myadlm.org/science-and-research/data-analytics-in-laboratory-medicine/fairlabs-challenge"),
             DTOutput("tbl"),
             plotlyOutput("plot"),
             DTOutput("dempar"), br()),
    tabPanel("Your Data",
             br(),
             downloadLink("download_template", "Download Template"),
             br(), br(),
             fileInput("dat", "Upload your data (.csv file)", accept = c(".csv")),
             DTOutput("tbl2"),
             plotlyOutput("plot2"),
             DTOutput("dempar2"), br())
    )
)

server <- function(input, output) {
  
  output$tbl <- renderDT({
    datatable(uds_dat,
              class = 'cell-border stripe',
              options = list(pageLength = 15, 
                             lengthChange = FALSE,
                             dom = "t",
                             ordering=F,
                             columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(uds_dat)-1)))), 
              rownames= FALSE)
  })
  
  output$tbl2 <- renderDT({
    req(input$dat)
    datatable(input$dat,
              class = 'cell-border stripe',
              options = list(pageLength = 15, 
                             lengthChange = FALSE,
                             dom = "t",
                             ordering=F,
                             columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(input$dat)-1)))), 
              rownames= FALSE)
  })
  
  output$plot <- renderPlotly(make_funnelplot(uds_dat))
  
  output$plot2 <- renderPlotly({
    req(input$dat)
    inFile <- input$dat
    dat <- read.csv(inFile$datapath, header = TRUE)
    make_funnelplot(dat)
    })
  
  output$dempar <- renderDT({
    datatable(check_demographic_parity(uds_dat),
              class = 'cell-border stripe',
              options = list(pageLength = 15, 
                             lengthChange = FALSE,
                             dom = "t",
                             ordering=F,
                             columnDefs = list(list(className = 'dt-center', targets = 0:1))), 
              rownames= FALSE)
  })
  
  output$dempar2 <- renderDT({
    req(input$dat)
    inFile <- input$dat
    dat <- read.csv(inFile$datapath, header = TRUE)
    datatable(check_demographic_parity(dat),
              class = 'cell-border stripe',
              options = list(pageLength = 15, 
                             lengthChange = FALSE,
                             dom = "t",
                             ordering=F,
                             columnDefs = list(list(className = 'dt-center', targets = 0:1))), 
              rownames= FALSE)
  })
  
  template <- read.csv("../template.csv")
  
  output$download_template <- downloadHandler(
    filename = function() { "equity_calculator_template.csv" },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
