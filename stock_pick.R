stock_list = function(ind, vol=1e9, cap = 1e9, s_date, n_month =12, N = 2){
  
  start_date = as.numeric(as.POSIXct(s_date, origin="1970-01-01"))
  end_date = as.numeric(as.POSIXct(AddMonths(s_date, n_month+1), origin="1970-01-01"))
  
  
  stockList = read.csv("nasdaq.csv",
                      #source: "https://public.opendatasoft.com/explore/dataset/nasdaq-companies/download/?format=csv&timezone=Europe/Berlin&lang=en&use_labels_for_header=true&csv_separator=%3B", 
                       sep = ";",
                       stringsAsFactors = FALSE)
  
  list1 = stockList[stockList$Sector %in% ind,]
  list2 = list1[list1$MarketCap >= cap, c('Symbol','Sector')]
  
  return(list2$Symbol)
} #end stock_list

  
get_quote = function(s, s_date,n_month, vol, sector){
  
  start_date = as.numeric(as.POSIXct(s_date, origin="1970-01-01"))
  end_date = as.numeric(as.POSIXct(AddMonths(s_date, n_month+1), origin="1970-01-01"))
  
  symbol_f = c()
  sector_f = c()
  vol_avg_f= c()
  growth_f = c()
  
  message(s)
  data = get_data(s, start_date, end_date)
  
  if (class(data) == 'character'){
    list3 = data.frame(symbol_f = numeric(0),
                        sector_f=numeric(0),
                        vol_avg_f=numeric(0),
                        growth_f=numeric(0))
    
  } else if(length(data$date) < n_month+1){
    list3 = data.frame(symbol_f = numeric(0),
                       sector_f=numeric(0),
                       vol_avg_f=numeric(0),
                       growth_f=numeric(0))
    
  } else {
    vol_avg = mean(data$vol[1:(length(data$date)-1)])
    if(vol_avg >= vol){
      symbol_f = c(symbol_f, s)
      sector_f = c(sector_f, sector)
      vol_avg_f = c(vol_avg_f, vol_avg)
      
      growth =  log(tail(data,1)[,'adj_close'] / data[1, 'adj_close'])
      growth_f= c(growth_f, growth)
      
    } else{
      # print('ccc')
      list3 = data.frame(symbol_f = numeric(0),
                         sector_f=numeric(0),
                         vol_avg_f=numeric(0),
                         growth_f=numeric(0))
    }# end if
  }# end if
  
  list3 = data.frame(symbol_f,sector_f,vol_avg_f, growth_f, stringsAsFactors = FALSE)
  
  return(list3)     
}#end function get_quote

get_top_stocks = function(list3, N){
  list4 = list3 %>% group_by(sector_f) %>% top_n(N, growth_f)
  return(list4)
}




