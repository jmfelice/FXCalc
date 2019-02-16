library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(dygraphs)
library(DT)
library(quantmod)
library(magrittr) 
library(dplyr)
library(shinyWidgets)
library(lubridate)
library(rmarkdown)
library(prettydoc)

currencies <- read.csv("currency_tables.csv")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel(title=div(img(src="background.jpg", height = "10%"), id = "header-img")),
  titlePanel(tags$div(id = "title", "Forex Report")),
  br(), br(), br(), br(),
  fluidRow(
    column(1, selectInput("currency", NULL, choices = currencies$Name, width = "100px")), 
    column(1, downloadButton("generate", "Generate Report", width = "30px"))
      ),
  br(), br(),
  HTML("<center>"),
  wellPanel(fluidRow(
    h3("Annual"), 
    column(4, DTOutput("annual.data")), 
    br(),
    br(),
    column(8, dygraphOutput("annual.graph", width = "75%"))
    )), 
  hr(),
  wellPanel(fluidRow(
    h3("Quarterly"), 
    column(4, DTOutput("quarterly.data")), 
    br(),
    br(),
    column(8, dygraphOutput("quarterly.graph", width = "75%"))
    )), 
  HTML("</center>")
  )
  

server <- function(input, output, session) {
  
  currency <- reactive({
    selected <- input$currency
    
    x <- currencies[which(currencies$Name == selected), 1] %>% as.character()
    
    x <- getSymbols(
      x, 
      src = "FRED", 
      getsymbols.warning4.0 = FALSE, 
      auto.assign = FALSE
      )
    
    x <- list("data" = x, "names" = selected)
  })
  
  qd <- reactive({
    quarterly <- currency()$data %>% quarterlyReturn()
    names(quarterly) <- currency()$names
    return(quarterly)
  })
  output$quarterly.graph <- renderDygraph({
    dygraph(qd()) %>% 
      dyRangeSelector() %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })
  output$quarterly.data <- renderDT({
    y <- qd() %>% tail(10) %>% index() %>% year
    q <- qd() %>% tail(10) %>% index() %>% quarter
    rnms <- paste0(y, " Q", q)
    
    qd() %>%
      tail(10) %>%
      datatable(
        options = list(
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        ), 
        class = "display-table",
        rownames = rnms
      ) %>%
      formatPercentage(columns = 1, digits = 3)
  })
  
  ad <- reactive({
    annual <- currency()$data %>% annualReturn()
    names(annual) <- currency()$names
    return(annual)
  })
  output$annual.graph <- renderDygraph({
    dygraph(ad()) %>% 
      dyRangeSelector() %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
  })
  output$annual.data <- renderDT({
    rnms <- ad() %>% tail(10) %>% index() %>% year %>% paste0
    
    ad() %>% 
      tail(10) %>%
      datatable(
        options = list(
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        ),
        class = "display-table",
        rownames = rnms
      ) %>%
      formatPercentage(columns = 1, digits = 3)
  })
  
  output$generate <- downloadHandler(
      filename = function() {paste("currency_report", Sys.Date(), ".html")}, 
      content = function(file) {
        out <- render('currency_report.Rmd', html_pretty())
        file.rename(out, file)
      }
    )
  
}

shinyApp(ui, server)