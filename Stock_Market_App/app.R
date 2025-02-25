library(shiny)
library(shinydashboard)
library(quantmod)
library(plotly)
library(DT)  

nasdaq_stocks <- stockSymbols(exchange = "NASDAQ")

ui <- dashboardPage(
  dashboardHeader(title = "Personal Finances"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # This ID will track the selected tab
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analyze", tabName = "analyze", icon = icon("chart-bar")),
      selectizeInput(
        "Portfolio",
        "Enter a List of Stocks",
        choices = nasdaq_stocks$Symbol,
        multiple = TRUE,
        options = list(create = TRUE, closeAfterSelect = TRUE),
        selected = "AAPL"
      ),
      radioButtons(
        "date_range",
        "Select Date Range:",
        choices = list(
          "1 Week" = "1w",
          "1 Month" = "1m",
          "3 Months" = "3m",
          "6 Months" = "6m",
          "1 Year" = "1y",
          "5 Years" = "5y",
          "Max" = "max"
        ),
        selected = "1m"
      ),
      # Conditional panel for "Analyze" tab
      conditionalPanel(
        condition = "input.tabs == 'analyze'",  # Show only for "Analyze" tab
        checkboxGroupInput(
          "indicators",
          "Select Indicators:",
          choices = c(
            "Revenues" = "rev",
            "Price Earning Ratios" = "pe",
            "Volumes Accumulation" = "va"
          ),
          selected = NULL
        )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Today's Stock Performance"),
              DTOutput("stock_table"), 
              h2("Interactive Stock Price Plot"),
              plotlyOutput("stock_plot") 
      ),
      
      tabItem(tabName = "analyze",
              h2("Stock Analysis")
      )
    )
  )
)


server <- function(input, output) {
  
  reactive_start_date <- reactive({
    end_date <- Sys.Date()
    switch(
      input$date_range,
      "1w" = end_date - 7,
      "1m" = end_date - 30,
      "3m" = end_date - 90,
      "6m" = end_date - 180,
      "1y" = end_date - 365,
      "5y" = end_date - 1825,
      "max" = as.Date("1900-01-01") 
    )
  })
  
  stock_data_reactive <- reactive({
    req(input$Portfolio)
    
    stock_info <- data.frame()
    
    return_label <- switch(
      input$date_range,
      "1w" = "Week Return",
      "1m" = "Month Return",
      "3m" = "3 Months Return",
      "6m" = "6 Months Return",
      "1y" = "Year Return",
      "5y" = "5 Years Return",
      "max" = "Max Return"
    )
    
    for (stock_symbol in input$Portfolio) {
      stock_data <- getSymbols(stock_symbol, src = "yahoo", from = reactive_start_date(), to = Sys.Date(), auto.assign = FALSE)
      stock_df <- data.frame(Date = index(stock_data), 
                             Open = as.numeric(Op(stock_data)), 
                             Close = as.numeric(Cl(stock_data)),
                             Volume = as.numeric(Vo(stock_data)))
      
      today_return <- (stock_df$Close[length(stock_df$Close)] - stock_df$Open[length(stock_df$Open)]) / stock_df$Open[length(stock_df$Open)]
      
      historical_price <- NA
      historical_date <- Sys.Date() - switch(
        input$date_range,
        "1w" = 7,
        "1m" = 30,
        "3m" = 90,
        "6m" = 180,
        "1y" = 365,
        "5y" = 1825,
        "max" = 365 * 100 
      )
      
      closest_date_idx <- which.min(abs(stock_df$Date - historical_date))
      historical_price <- stock_df$Close[closest_date_idx]
      
      stock_return <- (stock_df$Close[length(stock_df$Close)] - historical_price) / historical_price
      
      stock_info <- rbind(stock_info, 
                          data.frame(Stock = stock_symbol, 
                                     Volume = stock_df$Volume[length(stock_df$Volume)],
                                     Open = round(stock_df$Open[length(stock_df$Open)], 2),
                                     Close = round(stock_df$Close[length(stock_df$Close)], 2),
                                     Today_Return = paste0(round(today_return * 100, 2), "%"),
                                     Stock_Return = paste0(round(stock_return * 100, 2), "%")
                          )
      )
    }
    
    colnames(stock_info)[colnames(stock_info) == "Stock_Return"] <- return_label
    
    stock_info
  })
  
  # Render the plotly plot for selected stocks
  output$stock_plot <- renderPlotly({
    req(input$Portfolio)  
    
    p <- plot_ly()
    
    for (stock_symbol in input$Portfolio) {
      stock_data <- getSymbols(stock_symbol, src = "yahoo", from = reactive_start_date(), to = Sys.Date(), auto.assign = FALSE)
      stock_df <- data.frame(Date = index(stock_data), Price = as.numeric(Cl(stock_data)))
      
      p <- p %>%
        add_trace(
          data = stock_df,
          x = ~Date,
          y = ~Price,
          type = 'scatter',
          mode = 'lines',
          name = stock_symbol,
          text = paste("Date:", stock_df$Date, "<br>Price:", round(stock_df$Price, 2)),
          hoverinfo = "text"
        )
    }
    
    p %>%
      layout(
        title = "Stock Prices",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)")
      )
  })
  
  # Plot: Revenues
  output$revenue_plot <- renderPlotly({
    req(input$Portfolio, "rev" %in% input$indicators)
    p <- plot_ly()
    stock_data <- stock_data_reactive()
    
    for (symbol in names(stock_data)) {
      df <- stock_data[[symbol]]
      df$Revenues <- df$Price * runif(nrow(df), 0.8, 1.2)  # Dummy logic
      p <- p %>%
        add_trace(
          data = df,
          x = ~Date,
          y = ~Revenues,
          type = 'scatter',
          mode = 'lines',
          name = paste(symbol, "Revenues")
        )
    }
    
    p %>% layout(title = "Revenues", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
  })
  
  output$pe_ratio_plot <- renderPlotly({
    req(input$Portfolio, "pe" %in% input$indicators)
    p <- plot_ly()
    stock_data <- stock_data_reactive()
    
    for (symbol in names(stock_data)) {
      df <- stock_data[[symbol]]
      df$PE_Ratio <- df$Price / runif(nrow(df), 10, 30)  # Dummy logic
      p <- p %>%
        add_trace(
          data = df,
          x = ~Date,
          y = ~PE_Ratio,
          type = 'scatter',
          mode = 'lines',
          name = paste(symbol, "P/E Ratio")
        )
    }
    
    p %>% layout(title = "Price Earning Ratios", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
  })
  
  # Plot: Volume Accumulation
  output$volume_accumulation_plot <- renderPlotly({
    req(input$Portfolio, "va" %in% input$indicators)
    p <- plot_ly()
    stock_data <- stock_data_reactive()
    
    for (symbol in names(stock_data)) {
      df <- stock_data[[symbol]]
      df$Volume_Accumulation <- cumsum(runif(nrow(df), 100, 1000))  # Dummy logic
      p <- p %>%
        add_trace(
          data = df,
          x = ~Date,
          y = ~Volume_Accumulation,
          type = 'scatter',
          mode = 'lines',
          name = paste(symbol, "Volume Accumulation")
        )
    }
    
    p %>% layout(title = "Volume Accumulation", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
  })
  
  output$stock_table <- renderDT({
    stock_info <- stock_data_reactive()
    datatable(stock_info, options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)

