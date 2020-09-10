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



