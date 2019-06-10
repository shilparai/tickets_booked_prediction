setwd("D:/project/ticket_booked_pred")

# load libraries
library(forecast)
library(imputeTS)
library(Metrics)
library(smooth)

# time series model for channel_id and country and group by "type"

ts_data = data_1 %>% group_by(channel_id,country_1,date,key,class) %>%
  summarise(n_tickets = sum(n_tickets))

ts_data = ts_data[order(ts_data$key,ts_data$date),]
key_list = unique(ts_data$key)
output1 = data.frame()
output2 = data.frame()

# Building time series model 
for(prod_ID in 1:length(key_list)){
  
  tryCatch({
    
    indiv_prod_data = subset(ts_data, ts_data$key==key_list[prod_ID]) # get data for each key
    
    tsData = ts(indiv_prod_data$n_tickets, frequency = 7)
    #tsData = tsclean(tsData, replace.missing = TRUE, lambda = NULL)
    
    if(indiv_prod_data$class[1] =="A"){
      
      trainIndex =ceiling(nrow(indiv_prod_data)*0.80)
      
      train = window(tsData,end=c(floor(trainIndex)/7,6))
      fit = ses(train,alpha = 0.99)
      train = ceiling(fit$fitted)
      test = window(tsData,start=c(end(train)[1],end(train)[2]+1))
      # length(tsData)-length(train)-length(test) # check
      
      # ARIMA without exo-variables
      arima.fit <- auto.arima(train)
      arima_fcst_test <- forecast(arima.fit, h=length(test))$mean
      arima_fcst_test = ceiling(as.vector(arima_fcst_test))
      
      arima_train_error = mae(predicted = arima.fit$fitted, actual = arima.fit$x)
      
      # HoltWinters
      hw.fit = hw.fit = tryCatch(HoltWinters(train), error=function(err) ets(train))
      hw_fcst_test = forecast(hw.fit, h=length(test))$mean
      hw_fcst_test = ceiling(as.vector(hw_fcst_test))
      
      hw_train_error = mae(predicted = hw.fit$fitted[,1],actual = hw.fit$x)
      
      
      # Moving Average
      
      sma_fcst_test = sma(train, h = length(test))
      sma_train_error = mae(predicted = sma_fcst_test$fitted, actual = sma_fcst_test$actuals)
      sma_fcst_test = ceiling(as.vector(sma_fcst_test$forecast))
      
      
      # combined fcst
      
      combined_dt = indiv_prod_data[(length(train)+1):nrow(indiv_prod_data),]
      combined_dt$arima_fcst_test = arima_fcst_test
      combined_dt$arima_train_error = arima_train_error
      combined_dt$hw_fcst_test = hw_fcst_test
      combined_dt$hw_train_error = hw_train_error
      combined_dt$sma_fcst_test = sma_fcst_test
      combined_dt$sma_train_error = sma_train_error
      combined_dt = data.frame(combined_dt)
      
      output1 = rbind(output1, combined_dt)
      
      rm(combined_dt)
      rm(indiv_prod_data,tsData,trainIndex,train, test, arima.fit,arima_fcst_test,
         hw.fit,hw_fcst_test)
      
    } 
    else{
      
      # SES
      ses_fcst_test = ses(tsData,h = forecasting_period,
                          alpha = 0.9,
                          initial = "simple",
                          exponential=TRUE)$mean
      
      ses_fcst_test = ceiling(as.vector(ses_fcst_test))
      
      # Simple Moving Average
      
      sma_fcst_test = tryCatch(sma(tsData, h = forecasting_period)$forecast, error=function(err) ses(tsData,h = forecasting_period,
                                                                                                     alpha = 0.9,
                                                                                                     initial = "simple",
                                                                                                     exponential=TRUE)$mean)
      
      sma_fcst_test = ceiling(as.vector(sma_fcst_test))
      
      # combined fcst
      
      combined_dt2 = data.frame(key=rep(indiv_prod_data$key[1],forecasting_period))
      combined_dt2$date = seq.Date(max(indiv_prod_data$date)+1,by = "day",length.out = forecasting_period)
      combined_dt2$ses_fcst_test = ses_fcst_test
      combined_dt2$sma_fcst_test = sma_fcst_test
      
      combined_dt2 = data.frame(combined_dt2)
      output2 = rbind(output2,combined_dt2)
      
      rm(combined_dt2)
      
    }
    
    cat("COMPLETED FOR: ",prod_ID," -> ",key_list[prod_ID],"\n")
    
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
