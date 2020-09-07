#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# cd Documents/DS_4Sem/RR/Project
# ssh -i "rr_project.pem" ubuntu@ec2-3-122-195-55.eu-central-1.compute.amazonaws.com 
# ec2-18-185-249-57.eu-central-1.compute.amazonaws.com

if (!require("shiny")) install.packages("shiny")
if (!require("rjson")) install.packages("rjson")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DescTools")) install.packages("DescTools")
if (!require("purrr")) install.packages("purrr")
if (!require("quantmod")) install.packages("quantmod")
if (!require("profvis")) install.packages("profvis")
if (!require("compiler")) install.packages("compiler")
if (!require("multcomp")) install.packages("multcomp")



library(shiny)
library(rjson)
library(dplyr)
library(DescTools)
library(purrr)
library(quantmod)
library(profvis)
library(compiler) # compile is a base package installed by default
library(multcomp)



options(scipen = 100000000)

#================================================================================================================================================================
#### 1. function to get stock price from Yahoo Finance over a period of time ####

get_data = function(symbol,start_date, end_date){
    out = tryCatch(
        expr = {
            
            url = paste("https://query1.finance.yahoo.com/v7/finance/chart/",
                        symbol,
                        "?&interval=1mo&period1=",
                        start_date,
                        "&period2=",
                        end_date,
                        sep = '')
            
            data = fromJSON(file = url)
            date = as.Date(as.POSIXct(data$chart$result[[1]]$timestamp, origin="1970-01-01")) # a vector
            vol = data$chart$result[[1]]$indicators$quote[[1]]$volume
            adj_close = data$chart$result[[1]]$indicators$adjclose[[1]]$adjclose
            
            prices = data.frame(date, adj_close, vol)
        }, # end expr 
        error = function(e) {
            message('Stock data not available')
            return('Stock data not available')
        },
        warning = function(w){
            message('Stock data not available')
            return('Stock data not available')
        },
        finally = { NULL}
        
    )
    return(out)
}# end get_data() function

#================================================================================================================================================================
#### 2. function to filter stocks ####

# input:
#   - IND: industry name
#         a vector with values:
#     "Health Care"           "Finance"              
#     "Consumer Services"     "Technology"           
#     "Capital Goods"         "Energy"               
#     "Public Utilities"      "Basic Industries"     
#     "Transportation"        "Consumer Non-Durables"
#     "Consumer Durables"     "Miscellaneous"  

#   - CAP: miimum market cap (current) threshod
#   - VOL: minimum daily trading volume threshod over period of M months
#   - N: take top N stocks ranked by growth rate from start_date to end_date


stock_pick = function(ind, vol=1e9, cap = 1e9, s_date, n_month =12, N = 2){
    
    # s_date = "2017-09-01"
    # ind = c('Technology')
    # vol = 1e6
    # cap = 1e9
    # N = 2
    # n_month = 12
    
    start_date = as.numeric(as.POSIXct(s_date, origin="1970-01-01"))
    end_date = as.numeric(as.POSIXct(AddMonths(s_date, n_month+1), origin="1970-01-01"))
    
    
    stockList = read.csv("https://public.opendatasoft.com/explore/dataset/nasdaq-companies/download/?format=csv&timezone=Europe/Berlin&lang=en&use_labels_for_header=true&csv_separator=%3B", 
                         sep = ";",
                         stringsAsFactors = FALSE)
    
    list1 = stockList[stockList$Sector %in% ind,]
    list2 = list1[list1$MarketCap >= cap, c('Symbol','Sector')]
    
    
    #calculate avg trading volume and growth from start_date to end_date of each stock in list 2
    
    symbol_f = c()
    sector_f = c()
    vol_avg_f= c()
    growth_f = c()
    
    i = 1
    
    for(s in list2$Symbol){
        # s = list2$Symbol[65]
        
        message(paste(i, '/', length(list2$Symbol),' ', s))
        data = get_data(s, start_date, end_date)
        i = i+1
        if (class(data) == 'character'){
            # print('aaa')
            next
            
        } else if(length(data$date) < n_month+1){
            # print('bbb')
            next
            
        } else {
            vol_avg = mean(data$vol[1:(length(data$date)-1)])
            if(vol_avg >= vol){
                symbol_f = c(symbol_f, s)
                sector_f = c(sector_f, list2$Sector[list2$Symbol == s])
                vol_avg_f = c(vol_avg_f, vol_avg)
                
                growth =  log(tail(data,1)[,'adj_close'] / data[1, 'adj_close'])
                growth_f= c(growth_f, growth)
                
            } else{
                # print('ccc')
                next
            }# end if
        }# end if
        
    } # end for loop
    
    list3 = data.frame(symbol_f,sector_f,vol_avg_f, growth_f, stringsAsFactors = FALSE)
    
    list4 = list3 %>% group_by(sector_f) %>% top_n(N, growth_f)
    
    return(list4)                  
} #end function stock_pick


#================================================================================================================================================================
#### 3. Set up price-weighted portfolio ####

portfolio = function(stocks, 
                     s_date, 
                     n_month = 12,
                     rebalance = 3,
                     init_quantity = 10,
                     rf = 0.06){
    
    # s_date = "2018-07-01"
    # stocks = c("RP", "SANM") #s_f$symbol_f
    # rebalance = 3
    # init_quantity = 10
    # rf = 0.06
    # n_month =12
    
    
    start_date =as.numeric(as.POSIXct(s_date, origin="1970-01-01"))
    end_date =as.numeric(as.POSIXct(AddMonths(s_date, n_month+1), origin="1970-01-01"))
    
    
    #get prices of stock in the portfolio
    price = list()
    for ( s in stocks){
        p = get_data(s, start_date, end_date)
        price[[s]] = p
    }
    
    #flatten the list to data frame
    data =price %>% lapply(., function(x) x%>% dplyr::select(date, adj_close)) %>% 
        reduce(., merge, by = 'date', all = TRUE)
    
    names(data) =c('date', stocks)
    
    
    #set initial weight
    sum_price = sum(data[1,stocks])
    w = round(data[1, stocks] / sum_price,2)
    #initial quantity per stock = 10
    q0 = rep(init_quantity, length(stocks))
    # initial portfolio value
    p0 = sum(q0*data[1,stocks])
    
    
    #rebalance after xx months
    
    r_dt = AddMonths(data$date[1], rebalance) 
    q = q0
    pv = c(p0)
    
    for(t in 2: (length(data$date)-1)){
        
        
        if(data$date[t] != r_dt){
            
            #price of stock at date t EOD
            p_t = data[t+1, stocks]
            
            #portfolio value at date dt:
            pv = c(pv, sum(q*p_t))
            
        }else{
            
            #price of stock to rebalance
            p_r = data[t, stocks]
            
            # recalculate stock quantity
            q = pv[t-1]*w/p_r
            
            #price of stock at date t EOD
            p_t = data[t+1, stocks]
            
            #portfolio value at date dt EOD:
            pv = c(pv,sum(q*p_t))
            
            #next rebalance date
            r_dt = AddMonths(r_dt, rebalance) 
            
        } #end if
    } #end for loop
    
    
    PnL = tail(pv,1)*exp(-rf) - pv[1]
    PnL_diff = diff(pv, lag = 1)
    
    date = c(data$date[1:n_month])
    cumPnL = c(0,cumsum(PnL_diff))
    
    cumlative_PnL = data.frame(date = date, cumPnL =cumPnL )
    # 
    return_rate = log(tail(pv,1)*exp(-rf)/pv[1])
    #
    r = log(c(pv[2:length(pv)]/pv[1:(length(pv)-1)]))
    Shapre_ratio = (return_rate-rf)/sqrt(var(r))
    
    
    library(ggplot2)
    
    plt  =ggplot(cumlative_PnL, aes(x = date, y = cumPnL)) +
        geom_area(aes(fill=""),
                  alpha = 0.5, position = position_dodge(0.8)) +
        theme(legend.position = "none") +
        ggtitle("Cummulative profit & loss")
    
    
    rs = list(PnL = PnL, return_rate = return_rate, Sharpe_ratio = Shapre_ratio)
    
    print(plt)
    # return(rs)
    return(plt)
} #end function portfolio



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    stock_symbol <- eventReactive(input$submit_loc,{
                     stock_pick(c(input$sector1), 
                           vol = input$vol,
                           cap = input$cap, 
                           s_date = input$sdate, 
                           n_month = input$nmonth, 
                           N = input$N)
    })
    
    #render table of stocks
    output$stocks = renderTable({
        stock_symbol()
        })
    
    
    # draw the cummulative PnL
    output$portfolioPlot <- renderPlot({
        
        portfolio (stocks = stock_symbol()$symbol_f, #c("RP", "SANM"), 
                   s_date = input$p_sdate, 
                   n_month = input$p_nmonth,
                   rebalance = input$rebalance, #yearly
                   init_quantity = input$init_quantity,
                   rf = input$rf) 

    })

})
