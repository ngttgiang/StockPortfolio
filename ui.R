library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  #Application title
  hr(),
  
  titlePanel("Customize your portfolio:"),
  
  fluidRow(
    column(5,
           h4("Lookback period"),
           #select sector
           selectInput(inputId = 'sector1',
                       label = 'Sector',
                       choices = c('Finance'= 'Finance', 
                                   'Technology'='Technology', 
                                   'Health Care'='Health Care',
                                   'Energy' = 'Energy'),
                       multiple = FALSE,
                       selected = 'Technology'
           ),
           
           # input number of trading volume
           numericInput(inputId = "vol",
                        label = "Threshold of trading volume",
                        value = 1e6
           ),
           
           # input number of market cap
           numericInput(inputId = "cap",
                        label = "Threshold of market cap(USD)",
                        value = 1e11
           ),
           
           
           # text input for s_date
           textInput(inputId = "sdate",
                     label = "Start date",
                     value = "2017-01-01",
                     placeholder = 'Enter a date yyyy-mm-dd'
           ),
           
           
           # input number of month
           numericInput(inputId = "nmonth",
                        label = "Number of months from lookback start date ",
                        value = 12,
                        min = 1,
                        max = 12,
                        step = 1
           ),
           
           # input number of stock to pick
           numericInput(inputId = "N",
                        label = "Number of stocks to pick",
                        value = 2,
                        min = 1,
                        max = 5,
                        step = 1
           ),
           
           
           actionButton(
             inputId = 'submit_loc',
             label = "Submit"
           )
           
    ),
    column(5, h4("Investment period"), offset = 1,
           # text input for s_date portfolio
           textInput(inputId = "p_sdate",
                     label = "Start date",
                     value = "2018-01-01",
                     placeholder = 'Enter a date yyyy-mm-dd'
           ),
           
           # input number of month
           numericInput(inputId = "p_nmonth",
                        label = "Number of holding months",
                        value = 12,
                        min = 1,
                        max = 12,
                        step = 1
           ),
           
           # input number of rebalance frequency
           numericInput(inputId = "rebalance",
                        label = "Rebalance frequency",
                        value = 3,
                        min = 1,
                        max = 12,
                        step = 1
           ),
           
           # input number of initial quantity
           numericInput(inputId = "init_quantity",
                        label = "Initial invested quantity",
                        value = 10
           ),
           
           # Risk free rate
           numericInput(inputId = "rf",
                        label = "Risk-free rate",
                        value = 0.06
           ),
    ),
       
  ),
  
  hr(),
  
  
  mainPanel(
    textOutput('state'),
    hr(),
    textOutput('noStock'),
    # textOutput('numstock'),
    
    conditionalPanel( condition = "output.state =='Investment Portfolio Details'",
                     conditionalPanel(condition = "output.noStock != 'There are no stocks meet the provided criteria'",
                                      #selected stocks
                                      tableOutput("stock_table"),
                                      
                                      # Portfolio performance ratio
                                      tableOutput('portfolio_ratios'),
                                      
                                      #plot of cumulative PnL
                                      plotOutput('plot')
                                      )
                      )
    
    )#end main panel
  )#fluidPage
)#end UI

