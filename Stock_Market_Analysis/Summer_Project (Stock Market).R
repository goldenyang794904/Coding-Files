library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(quantmod)

popular_tickers <- c("AAPL", "GOOG", "MSFT", "AMZN", "FB", "TSLA", "NFLX", "BABA", "JPM", 
                     "V", "DIS", "PFE", "KO", "PEP", "CSCO", "INTC", "MRK", "WMT", "BA")

ui <- navbarPage("Financial Dashboard",
                 tabPanel("Home",
                          fluidPage(
                            titlePanel("Welcome to the Financial Dashboard"),
                            h3("Navigate through the tabs to explore various financial data."),
                            tags$img(src = "/Users/antonyang/Rplot15.png", alt = "Background Image", style = "width:100%;")                          )
                 ),
                 
                 tabPanel("Stock",
                          fluidPage(
                            tags$head(
                              tags$style(HTML("
                                .datepicker {
                                  z-index: 1050 !important;
                                }
                              "))
                            ),
                            titlePanel("Stock Market Check Up"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("selected_stock", "Select a Stock:", choices = popular_tickers, selected = popular_tickers[1]),
                                dateInput("start_date", "From:", value = Sys.Date() - 30),
                                dateInput("end_date", "To:", value = Sys.Date()),
                                actionButton("get_data", "Get Data")
                              ),
                              mainPanel(
                                tableOutput("stock_table"),
                                plotOutput("stock_plot")
                              )
                            )
                          )
                 )
)

server <- function(input, output) {
  observeEvent(input$get_data, {
    stock_symbol <- input$selected_stock
    
    stock_data <- getSymbols(stock_symbol, src = "yahoo", from = input$start_date, to = input$end_date, auto.assign = FALSE)
    
    output$stock_plot <- renderPlot({
      chartSeries(stock_data, name = stock_symbol, theme = chartTheme("white"))
    })
  })
}

shinyApp(ui, server)




