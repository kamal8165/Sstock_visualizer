#----
#title: "APPLICATION in R FOR THE CALCULATION OF DOW JONES TECHNICAL INDICATORS "
#author: "Kamal Pipaliya, Shivam mandaliya, mandar devta"
#date: "6/9/2022"
#----
install.packages('tidyverse')
install.packages('flexdashboard')
install.packages('shiny')
install.packages('shinydashboard')
install.packages('shinythemes')
install.packages('quantmod')
install.packages('DT')

library(tidyverse)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(quantmod)
library(DT)

ui <- dashboardPage(skin="red",
                    dashboardHeader (title = "Analyse technique DJI"),
                    dashboardSidebar(helpText("  Choose time interval"),
                                     dateRangeInput("dates","time interval",start = "2015-01-01",
                                                    end = as.character(Sys.Date())),
                                     br(),
                                     br(),
                                     sidebarMenu(
                                       menuItem("Predict Price", tabName = "PREDICT_PRICE", icon = icon("info")),
                                       
                                       br(),
                                       menuItem("Show data", tabName = "ViewData", icon = icon("info")),
                                       
                                       br(),
                                       
                                       
                                       menuItem("Trend indicators", tabName = "indtech", icon = icon("chart-line")),
                                       br(),
                                       menuItem("Graph", tabName = "graph", icon = icon("poll")) )
                    ),
                    
                    dashboardBody(img(src ="https://media.istockphoto.com/photos/times-square-picture-id585093718?k=20&m=585093718&s=612x612&w=0&h=cGNjqWfkOWwbDR1FKcxm8Bo733zMGROe6_f9WfyLG-A=", height = 200, width = 500),
                                  tabItems(
                                    
                                    tabItem(tabName = "PREDICT_PRICE",
                                            h1("Forecast AAPL stock price"),
                                            tabsetPanel(
                                              
                                              tabPanel("ARIMA Forecast for AAPL stock",plotOutput("table0") ) 
                                              
                                            )
                                            
                                    ),
                                    
                                    
                                    #graph
                                    tabItem(
                                      tabName = "graph",
                                      h1("Dowjones chart with indicators"),
                                      tabsetPanel(
                                        tabPanel("Graph complete", plotOutput("plot5",height= 500) )
                                      )
                                      
                                    ),
                                    # ViewData
                                    tabItem(tabName = "ViewData",
                                            h1("Dowjones data"),
                                            tabsetPanel(
                                              
                                              tabPanel("Classes",tableOutput("table1") ),   
                                              tabPanel("technical indicators",tableOutput("table2") )
                                            )
                                            
                                    ),
                                    
                                    
                                    
                                    #indtech
                                    tabItem(tabName ="indtech",
                                            h1("Some trend indicators"),
                                            tabsetPanel(
                                              tabPanel("Classes", plotOutput("plot",height= 500) ),
                                              tabPanel("SMA", plotOutput("plot1",height= 500) ),
                                              tabPanel("MACD", plotOutput("plot2",height= 500) ),
                                              tabPanel("RSI", plotOutput("plot3",height= 500) ),
                                              tabPanel("ROC", plotOutput("plot4",height= 500) )
                                              
                                              
                                            ) 
                                    )
                                    
                                  )
                    )
)

################# Stock Market Price Prediction using R #################

##Importing Required Packages
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)


##Importing Dataset from Finance Websites...(Default yahoo)
getSymbols('AAPL', from = '2019-01-01', to = '2021-01-01')
View(AAPL)
#class(AAPL)


chartSeries(AAPL, subset = 'last 6 months', type = 'auto')
addBBands()







server <- function(input, output) {
  
  output$table0 <- renderTable ( {
    dataInput<-getSymbols('AAPL', from = '2019-01-01', to = '2021-01-01')
    #View(AAPL)
    #class(AAPL)
    
    
    chartSeries(AAPL, subset = 'last 6 months', type = 'auto')
    addBBands()
    
    # functions
    
    ##Assigning columns of dataset  
    Open_prices = AAPL[,1]
    High_prices = AAPL[,2]
    Low_prices = AAPL[,3]
    Close_prices = AAPL[, 4]
    Volume_prices = AAPL[,5]
    Adjusted_prices = AAPL[,6]
    
    par(mfrow = c(2,3))
    
    plot(Open_prices, main = 'Opening Price of Stocks (Over a given period)')
    plot(High_prices, main = 'Highest Price of Stocks (Over a given period)')
    plot(Low_prices, main = 'Lowest Price of Stocks (Over a given period)')
    plot(Close_prices, main = 'Closing Price of Stocks (Over a given period)')
    plot(Volume_prices, main = 'Volume of Stocks (Over a given period)')
    plot(Adjusted_prices, main = 'Adjusted Price of Stocks (Over a given period)')
    
    Predic_Price = Adjusted_prices
    #class(Predic_Price)
    
    
    ######## Finding the Linear Relation between observations ########
    
    par(mfrow = c(1,2))
    Acf(Predic_Price, main = 'ACF for differenced Series')
    Pacf(Predic_Price, main = 'PACF for differenced Series ', col = '#cc0000')
    Auto_cf = Acf(Predic_Price, plot = FALSE)
    Auto_cf
    PAuto_cf = Pacf(Predic_Price, plot = FALSE)
    PAuto_cf
    
    print(adf.test(Predic_Price))
    
    
    
    
    ################### Prediction of Return ##########################
    
    return_AAPL <- 100*diff(log(Predic_Price))
    
    AAPL_return_train <- return_AAPL[1:(0.9*length(return_AAPL))]
    
    AAPL_return_test <- return_AAPL[(0.9*length(return_AAPL)+1):length(return_AAPL)]
    
    auto.arima(AAPL_return_train, seasonal = FALSE)
    
    fit <- Arima(AAPL_return_train, order = c(1,0,0))
    
    preds <- predict(fit, n.ahead = (length(return_AAPL) - (0.9*length(return_AAPL))))$pred
    preds
    
    
    ################## Forecasting Predicted Result ##################
    
    test_forecast <- forecast(fit,h = 15)
    test_forecast
    output$plot <- renderPlot({
      dataInput <- (test_forecast)
      
      
      chartSeries( dataInput,type="candlesticks",name="AAPL" ,theme=chartTheme('white'),addVo())
    }) 
    
   
  })
  output$plot <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    chartSeries( dataInput,type="candlesticks",name="^DJI" ,theme=chartTheme('white'),addVo())
    
  }) 
  
  output$table1 <- renderTable(dataInput <- getSymbols("^DJI", src = "yahoo",
                                                       from = input$dates[1],
                                                       to = input$dates[2],
                                                       period     = "daily",
                                                       auto.assign = FALSE))
  
  output$table2 <- renderTable ( {
    dataInput<-getSymbols("^DJI", src = "yahoo",
                          from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE) 
    
    # fonctions
    
    #simple moving average (SMA)                                          
    SMA20 <-SMA(Cl(dataInput),n=20) 
    colnames(SMA20) <- "SMA20"
    
    SMA50 <-SMA(Cl(dataInput),n=50)
    colnames(SMA50) <- "SMA50"
    #MACD
    macd<- MACD(Cl(dataInput), nFast=12, nSlow=26,
                nSig=9, percent=FALSE)
    
    #RSI
    rsi <- RSI(Cl(dataInput), SMA, n=14)
    colnames(rsi) <- "RSI"
    #Rate of Change (ROC)
    roc <- ROC(Cl(dataInput),n=2)
    colnames(roc) <- "ROC"
    data1 <-cbind(SMA20,SMA50,roc,rsi,macd,dataInput$date)
    data1
  })
  output$plot <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    chartSeries( dataInput,type="candlesticks",name="^DJI" ,theme=chartTheme('white'),addVo())
    
  }) 
  output$plot1 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme = chartTheme("black"), 
                 TA=c(
                   addVo(), 
                   addSMA(n=200,on=1, col = 'white'),
                   addSMA(n=50,on=1,col = 'orange'),
                   addSMA(n=20,on=1,col = 'red')
                 )
                 
    )
  })
  output$plot2 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    chartSeries (dataInput,name= "^DJI",theme = chartTheme('white'), 
                 TA=c(
                   addVo(), 
                   addMACD(fast=12,slow=26,signal=9,type="EMA")
                 ))
    
  })
  output$plot3 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme=chartTheme("black"), 
                 TA=c( addVo(), addRSI())
                 
    )
    
    
    
  })
  output$plot4 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme = chartTheme("white"), 
                 TA=c(addVo(), addROC(n=50,col = 'red'))
                 
                 
    )
    
    
  })
  output$plot5 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme = chartTheme("white"), 
                 TA=c(addVo(),addMACD(fast=12,slow=26,signal=9,type="EMA"),
                      addSMA(n=200,col = 'orange'),
                      addSMA(n=50,col = 'green'),
                      addSMA(n=20,col = 'black'),
                      addROC(n=50,col = 'red'),
                      addRSI()
                      
                      
                 ))
    
    
  })
}
shinyApp(ui, server)

