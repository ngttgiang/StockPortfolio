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
  
  
  rs = list(PnL = PnL, return_rate = round(return_rate*100,2), Sharpe_ratio = Shapre_ratio, plot = plt)
  
  print(plt)
  return(rs)
  # return(plt)
} #end function portfolio
