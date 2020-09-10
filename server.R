
#load packages
library(shiny)
library(rjson)
library(dplyr)
library(DescTools)
library(purrr)
library(profvis)
library(compiler) # compile is a base package installed by default
library(multcomp)

options(scipen = 100000000)

#================================================================================================================================================================
#### 1. function to get stock price from Yahoo Finance over a period of time ####

# input:
#   @symbol: stock ticker
#   @start_date: the date to start holding the portfolio
#   @end_date: the date to stop holding the portfolio

source('get_data_Yahoo 2.R')

#================================================================================================================================================================
#### 2. function to filter stocks ####

# input:
#   @ind: industry name
#         a vector with values:
#     "Health Care"           "Finance"              
#     "Consumer Services"     "Technology"           
#     "Capital Goods"         "Energy"               
#     "Public Utilities"      "Basic Industries"     
#     "Transportation"        "Consumer Non-Durables"
#     "Consumer Durables"     "Miscellaneous"  
#   @s_date: the date to start holding the portfolio
#   @cap: minimum market cap (current) threshold
#   @vol: minimum daily trading volume threshold over period of M months
#   @n_month: Number of months to hold stocks after the start date
#   @N: take top N stocks ranked by growth rate from start_date to end_date

source('stock_pick.R')

#================================================================================================================================================================
#### 3. Set up price-weighted portfolio ####

# input:
#   @stocks: the stocks in the portfolio
#   @s_date: the date to start holding the portfolio
#   @n_month: Number of months to hold stocks after the start date
#   @rebalance: the number of months after that the portfolio is rebalanced
#   @init_quantity: the volume invested for stocks in the portfolio at the beginning. 
#   @rf: the risk-free rate

source('portfolio_measure.R')

#================================================================================================================================================================
# Define server logic 
shinyServer(function(input, output) {
        
    pstate <- reactiveVal(numeric(0))
    
    num_stocks <- reactiveVal(numeric(0))
        

    
    
    selected_stocks <- reactiveVal(data.frame(symbol_f = numeric(0),
                                              sector_f=numeric(0),
                                              vol_avg_f=numeric(0),
                                              growth_f=numeric(0))
                                   )
    
    portfolio_measure <- reactiveVal(list(PnL = numeric(0), 
                                          return_rate = numeric(0),
                                          Sharpe_ratio = numeric(0), 
                                          plot = ggplot()))

    
    observeEvent(
        input$submit_loc,
        {
            dat <- data.frame(symbol_f = numeric(0),
                              sector_f=numeric(0),
                              vol_avg_f=numeric(0),
                              growth_f=numeric(0))
            
            nstocks = stock_list(ind = c(input$sector1),
                                vol = input$vol,
                                cap = input$cap,
                                s_date = input$sdate,
                                n_month = input$nmonth,
                                N = input$N)
            
            withProgress(message = 'Loading data', value = 0, {
                # Number of times we'll go through the loop
                
                for (i in 1:length(nstocks)) {
                    # Each time through the loop, add another row of data. This is
                    # a stand-in for a long-running computation.
                    quote  = get_quote(s = nstocks[i],
                                       s_date = input$sdate,
                                       n_month = input$nmonth,
                                       vol = input$vol,
                                       sector = input$sector1
                    )
                    dat <- rbind(dat, quote)
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/length(nstocks), detail = paste("...", round(100*i/length(nstocks),0),'% ... of total ',length(nstocks), 'stocks'))
                    
                    top_stocks = get_top_stocks(dat, N = input$N)
                    selected_stocks(top_stocks)
                    
                    
                }#end for
                
                pstate('Investment Portfolio Details')
                
            })#end progress
            
        }
    )#end observe event
    
    

    output$plot <- renderPlot({
        portfolio_measure(portfolio (stocks = selected_stocks()$symbol_f,
                                     s_date = input$p_sdate,
                                     n_month = input$p_nmonth,
                                     rebalance = input$rebalance,
                                     init_quantity = input$init_quantity,
                                     rf = input$rf))
        
        portfolio_measure()$plot
    })
    
    #render table of stocks in the portfolio
    output$stock_table = renderTable({
        invest_portfolio <- selected_stocks()
        invest_portfolio = as.data.frame(invest_portfolio)
        colnames(invest_portfolio) = c("Ticker", "Sector", "Avg. Trading volume", "Price growth")
        invest_portfolio
    })
    
    
    # render table of portfolio performance ratios
    output$portfolio_ratios = renderTable({
        p_ratios <- data.frame(PnL = c(portfolio_measure()$PnL),
                               Return_rate = c(portfolio_measure()$return_rate),
                               Sharpe_ratio = c(portfolio_measure()$Sharpe_ratio))

        colnames(p_ratios) = c("PnL(USD)", "Return rate(%)", "Sharpe Ratio")
        p_ratios
    })
    
    output$state = renderText(pstate())
    
    output$numstock = renderText({
        if (!is_empty(pstate())){
            num_stocks(nrow(selected_stocks()))
        }
        
        num_stocks()
    })
    
    output$noStock = renderText({
        if (nrow(selected_stocks()) == 0 & !is_empty(pstate())){
            ("There are no stocks meet the provided criteria")
        }
        else{
            numeric(0)
        }
        
    })

    
})
