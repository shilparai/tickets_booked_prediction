setwd("D:/project/ticket_booked_pred")

# load libraries
library(catboost)
library(h2o)
library(caret)
library(Metrics)
library(RcppRoll)
library(forecast)
library(smooth)

key_list = unique(data_1$key)

# data for machine learning models
ml_data = data_1 %>%
  group_by(key) %>%
  mutate(lag_1 = lag(n_tickets, 1) # 1- week lag
         , avg_7 = lag(roll_meanr(n_tickets, 7), 1) # - 7 days moving average
         , avg_3 = lag(roll_meanr(n_tickets, 3), 1) # 3- days moving average
  )

ml_data = ml_data[!is.na(ml_data$avg_7),]
ml_data$class = NULL

# generate training and testing data
train = data.frame()
test = data.frame()
list_for_ts = c()

for(prod_ID in 1:length(key_list)){
  
  indiv_prod_data = subset(ml_data, ml_data$key==key_list[prod_ID]) # get data for each key
  
  if(nrow(indiv_prod_data)>=300){
    
    #cat("Appending Data for :", key_list[prod_ID],"\n")
    trainIndex = ceiling(nrow(indiv_prod_data)*0.80)
    tr = indiv_prod_data[1:trainIndex,]
    tr = data.frame(tr)
    ts = indiv_prod_data[(trainIndex+1):nrow(indiv_prod_data),]
    ts = data.frame(ts)
    train = rbind(train,tr)
    test = rbind(test,ts)
    
  } else{
    list_for_ts = c(list_for_ts, key_list[prod_ID]) 
  }
}

list_for_ml = unique(ml_data$key)
key_list = unique(train$key)


# RANDOM FOREST MODEL-----------------------------------------------------------------

if(!file.exists("./output/rf_model_list.RData")){
  
  model_list = list(prod=c(),modeltest=list(),final_model = list()) # to save models
  metric_list = list(prod=c(),maeTest = list(),maeTrain = list())
  
  h2o.init(nthreads=-1)
  
  ## Load train, test data into cluster from R
  
  for(prod_ID in 1:length(key_list)){
    
    indiv_train = subset(train, train$key == key_list[prod_ID])
    indiv_test = subset(test, test$key == key_list[prod_ID])
    
    trainHex<-as.h2o(indiv_train)
    valHex<-as.h2o(indiv_test)
    
    rfHex <- h2o.randomForest(x=names(train[,vars]),
                              y="n_tickets", 
                              ntrees = 100,
                              training_frame=trainHex,
                              validation_frame = valHex,
                              nfolds = 3,
                              score_each_iteration = TRUE,
                              model_id = paste0("RFTest_",key_list[prod_ID]), seed = 999,
                              keep_cross_validation_predictions = TRUE)
    
    
    h2o_rf_predict = as.data.frame(h2o.predict(rfHex,valHex))
    h2o_rf_predict = ceiling(h2o_rf_predict)
    cor(indiv_test$n_tickets,h2o_rf_predict)
    cat("MAE for ",key_list[prod_ID]," : ",mae(indiv_test$n_tickets,h2o_rf_predict$predict),"\n")
    
    model_list$prod[prod_ID] = key_list[prod_ID]
    model_list$modeltest[[prod_ID]] = rfHex
    
    metric_list$prod[prod_ID] = key_list[prod_ID]
    perf = h2o.performance(rfHex)
    metric_list$maeTest[[prod_ID]] = mae(indiv_test$n_tickets,h2o_rf_predict$predict)
    metric_list$maeTrain[[prod_ID]] = perf@metrics$mae
    
    rm(h2o_rf_predict,trainHex,rfHex)
    
    # forecasting
    # generate forecast using model trained on all data
    
    trainHex<-as.h2o(rbind(indiv_train,indiv_test))
    
    rfHex <- h2o.randomForest(x=names(train[,vars]),
                              y="n_tickets", 
                              ntrees = 100,
                              training_frame=trainHex,
                              nfolds = 3,
                              score_each_iteration = TRUE,
                              keep_cross_validation_predictions = TRUE,
                              model_id = paste0("RF_",key_list[prod_ID]), seed = 1234)
    
    model_list$final_model[[prod_ID]] = paste0("RF_",key_list[prod_ID])
    h2o.saveModel(object = rfHex, "./h2o")
    
    cat("COMPLETE FOR : ",key_list[prod_ID])
  }
  
  save(model_list, file="./output/rf_model_list.RData")
  save(metric_list, file="./output/rf_metric_list.RData")
  
} else {
  h2o.init(nthreads=-1)
  load("./output/rf_model_list.RData")
}

## Forecating using random foreast

rf_fcst_output = data.frame()

for(prod_ID in 1:length(model_list$prod)){
  
  indiv_train = subset(ml_data, ml_data$key == model_list$prod[prod_ID])
  indiv_train = data.frame(indiv_train)
  indiv_train = indiv_train[order(indiv_train$date),]
  
  rfModel = model_list$final_model[[prod_ID]]
  rfModel = h2o.loadModel(paste0("./h2o/",rfModel))
  
  fcst_date = seq.Date(max(ml_data$date)+1,by="day",length.out = forecasting_period)
  
  indiv_fcst = data.frame(date=as.Date(fcst_date))
  indiv_fcst$Year = as.numeric(format(indiv_fcst$date,"%Y"))
  indiv_fcst$month = as.numeric(format(indiv_fcst$date,"%m"))
  indiv_fcst$weekday = (weekdays(indiv_fcst$date))
  indiv_fcst$day_of_month = as.integer(format(indiv_fcst$date,"%d"))
  indiv_fcst$isWeekend = as.numeric(ifelse(indiv_fcst$weekday=="Saturday" | indiv_fcst$weekday=="Sunday",1,0))
  indiv_fcst$isWeekday = as.numeric(ifelse(indiv_fcst$weekday=="Saturday" | indiv_fcst$weekday=="Sunday",0,1))
  indiv_fcst$month_end = as.numeric(ifelse(indiv_fcst$day_of_month>28,1,0))
  indiv_fcst$end_week_of_month = as.numeric(ifelse(indiv_fcst$day_of_month>23 & indiv_fcst$day_of_month<=31,1,0))
  indiv_fcst$start_week_of_month = as.numeric(ifelse(indiv_fcst$day_of_month>=1 & indiv_fcst$day_of_month<=7,1,0))
  indiv_fcst$channel_id = indiv_train$channel_id[1]
  indiv_fcst$country_1 = indiv_train$country_1[1]
  indiv_fcst$key = model_list$prod[prod_ID]
  
  indiv_fcst$lag_1=0
  indiv_fcst$avg_7=0
  indiv_fcst$avg_3=0
  indiv_fcst$n_tickets=0
  indiv_fcst$weekday = NULL
  
  for(i in 1:nrow(indiv_fcst)){
    
    if(i==1){
      
      indiv_fcst$lag_1[i] = indiv_train$n_tickets[nrow(indiv_train)]
      indiv_fcst$avg_7[i] = lag(roll_meanr(indiv_train$n_tickets, 7), 1)[nrow(indiv_train)]
      indiv_fcst$avg_3[i] = lag(roll_meanr(indiv_train$n_tickets, 3), 1)[nrow(indiv_train)]
      
      inputHex<-as.h2o(indiv_fcst[i,vars])
      indiv_fcst$n_tickets[i] = ceiling(as.data.frame(h2o.predict(rfModel,inputHex))$predict)
      
    }else{
      
      indiv_train = rbind(indiv_train,indiv_fcst[i-1,])
      indiv_fcst$lag_1[i] = indiv_train$n_tickets[nrow(indiv_train)]
      indiv_fcst$avg_7[i] = lag(roll_meanr(indiv_train$n_tickets, 7), 1)[nrow(indiv_train)]
      indiv_fcst$avg_3[i] = lag(roll_meanr(indiv_train$n_tickets, 3), 1)[nrow(indiv_train)]
      inputHex<-as.h2o(indiv_train[i,vars])
      indiv_fcst$n_tickets[i] = ceiling(as.data.frame(h2o.predict(rfModel,inputHex))$predict)
      
    }
    
  }
  
  hist_data = data.frame(subset(ml_data, ml_data$key == model_list$prod[prod_ID]))
  
  hist_data$flag = "Actual"
  indiv_fcst$flag = "Forecast"
  
  rf_fcst_output = rbind(rf_fcst_output,hist_data,indiv_fcst)
  cat("COMPLETE FOR : ",model_list$prod[prod_ID])
  rm(hist_data,indiv_fcst,rfModel)
  
}

write_csv(rf_fcst_output,"./output/rf_fcst_output.csv")

# CATBOOST MODEL-------------------------------------------------------------------

if(!file.exists("./output/catboost_model_list.RData")){
  
  model_list = list(prod=c(),modeltest=list(),final_model = list()) # to save models
  metric_list = list(prod=c(),maeTest = list(),maeTrain = list())
  
  for( prod_ID in 1:length(key_list)){
    
    indiv_train = subset(train, train$key == key_list[prod_ID])
    indiv_test = subset(test, test$key == key_list[prod_ID])
    
    y_train <- unlist(indiv_train[c('n_tickets')])
    X_train <- indiv_train %>% select(-n_tickets)
    X_train$key = NULL
    X_train$date = NULL
    X_train$Year = NULL
    X_train$channel_id = NULL
    X_train$country_1 = NULL
    
    
    y_valid <- unlist(indiv_test[c('n_tickets')])
    X_valid <- indiv_test %>% select(-n_tickets)
    X_valid$key = NULL
    X_valid$date = NULL
    X_valid$Year = NULL
    X_valid$channel_id = NULL
    X_valid$country_1 = NULL
    
    train_pool <- catboost.load_pool(data = X_train, label = y_train)
    test_pool <- catboost.load_pool(data = X_valid, label = y_valid)
    
    params <- list(iterations=1000,
                   learning_rate=0.03,
                   depth=10,
                   loss_function='RMSE',
                   eval_metric='RMSE',
                   random_seed = 55,
                   od_type='Iter',
                   metric_period = 50,
                   od_wait=20,
                   use_best_model=TRUE)
    set.seed(999)
    model <- catboost.train(learn_pool = train_pool,params = params)
    
    #predict
    
    y_pred=catboost.predict(model,test_pool)
    y_pred = ceiling(y_pred)
    
    model_list$prod[prod_ID] = key_list[prod_ID]
    model_list$modeltest[[prod_ID]] = model
    
    metric_list$prod[prod_ID] = key_list[prod_ID]
    metric_list$maeTrain[[prod_ID]] = mae(indiv_train$n_tickets,catboost.predict(model,train_pool))
    metric_list$maeTest[[prod_ID]] = mae(indiv_test$n_tickets,y_pred)
    
    #calculate error metrics
    
    cat("MAPE for ",key_list[prod_ID]," : ",postResample(y_pred,indiv_test$n_tickets),"\n")
    
    dt = rbind(indiv_train,indiv_test)
    dt = dt[order(dt$date),]
    
    y_train <- unlist(dt[c('n_tickets')])
    X_train <- dt %>% select(-n_tickets)
    X_train$key = NULL
    X_train$date = NULL
    X_train$Year = NULL
    X_train$channel_id = NULL
    X_train$country_1 = NULL
    
    rm(train_pool,model)
    
    train_pool <- catboost.load_pool(data = X_train, label = y_train)
    model <- catboost.train(learn_pool = train_pool,params = params)
    model_list$final_model[[prod_ID]] = model
    
  }
  
  
  save(model_list, file="./output/catboost_model_list.RData")
  save(metric_list, file="./output/catboost_metric_list.RData")
  
} else {
  load("./output/catboost_model_list.RData")
}

# Forecasting using catboost model

catboost_fcst_output = data.frame()

for(prod_ID in 1:length(model_list$prod)){
  
  indiv_train = subset(ml_data, ml_data$key == model_list$prod[prod_ID])
  indiv_train = data.frame(indiv_train)
  indiv_train = indiv_train[order(indiv_train$date),]
  
  model = model_list$final_model[[prod_ID]]
  
  fcst_date = seq.Date(max(ml_data$date)+1,by="day",length.out = forecasting_period)
  
  indiv_fcst = data.frame(date=as.Date(fcst_date))
  indiv_fcst$Year = as.numeric(format(indiv_fcst$date,"%Y"))
  indiv_fcst$month = as.numeric(format(indiv_fcst$date,"%m"))
  indiv_fcst$weekday = (weekdays(indiv_fcst$date))
  indiv_fcst$day_of_month = as.integer(format(indiv_fcst$date,"%d"))
  indiv_fcst$isWeekend = as.numeric(ifelse(indiv_fcst$weekday=="Saturday" | indiv_fcst$weekday=="Sunday",1,0))
  indiv_fcst$isWeekday = as.numeric(ifelse(indiv_fcst$weekday=="Saturday" | indiv_fcst$weekday=="Sunday",0,1))
  indiv_fcst$month_end = as.numeric(ifelse(indiv_fcst$day_of_month>28,1,0))
  indiv_fcst$end_week_of_month = as.numeric(ifelse(indiv_fcst$day_of_month>23 & indiv_fcst$day_of_month<=31,1,0))
  indiv_fcst$start_week_of_month = as.numeric(ifelse(indiv_fcst$day_of_month>=1 & indiv_fcst$day_of_month<=7,1,0))
  indiv_fcst$channel_id = indiv_train$channel_id[1]
  indiv_fcst$country_1 = indiv_train$country_1[1]
  indiv_fcst$key = model_list$prod[prod_ID]
  
  indiv_fcst$lag_1=0
  indiv_fcst$avg_7=0
  indiv_fcst$avg_3=0
  indiv_fcst$n_tickets=0
  indiv_fcst$weekday = NULL
  
  for(i in 1:nrow(indiv_fcst)){
    
    if(i==1){
      
      indiv_fcst$lag_1[i] = indiv_train$n_tickets[nrow(indiv_train)]
      indiv_fcst$avg_7[i] = lag(roll_meanr(indiv_train$n_tickets, 7), 1)[nrow(indiv_train)]
      indiv_fcst$avg_3[i] = lag(roll_meanr(indiv_train$n_tickets, 3), 1)[nrow(indiv_train)]
      
      test_pool <- catboost.load_pool(data = indiv_fcst[i,vars])
      indiv_fcst$n_tickets[i] = ceiling(catboost.predict(model,test_pool))
      
    }else{
      
      indiv_train = rbind(indiv_train,indiv_fcst[i-1,])
      indiv_fcst$lag_1[i] = indiv_train$n_tickets[nrow(indiv_train)]
      indiv_fcst$avg_7[i] = lag(roll_meanr(indiv_train$n_tickets, 7), 1)[nrow(indiv_train)]
      indiv_fcst$avg_3[i] = lag(roll_meanr(indiv_train$n_tickets, 3), 1)[nrow(indiv_train)]
      test_pool <- catboost.load_pool(data = indiv_fcst[i,vars])
      indiv_fcst$n_tickets[i] = ceiling(catboost.predict(model,test_pool)) 
    }
    
  }
  
  hist_data = data.frame(subset(ml_data, ml_data$key == model_list$prod[prod_ID]))
  
  hist_data$flag = "Actual"
  indiv_fcst$flag = "Forecast"
  
  catboost_fcst_output = rbind(catboost_fcst_output,hist_data,indiv_fcst)
  cat("COMPLETE FOR : ",model_list$prod[prod_ID],"\n")
  rm(hist_data,indiv_fcst,CBModel)
}

write_csv(catboost_fcst_output,"./output/catboost_fcst_output.csv")


# TIME SERIES FOR OTHERs -----------------------------------------------

ts_fcst_output = data.frame()

ml_ts_data = data_1 %>% group_by(channel_id,country_1,date,key) %>%
  summarise(n_tickets = sum(n_tickets))

ml_ts_data = ml_ts_data[order(ml_ts_data$key,ml_ts_data$date),]


for(prod_ID in 1:length(list_for_ts)){
  
  indiv_prod_data = subset(ml_ts_data, ml_ts_data$key==list_for_ts[prod_ID]) # get data for each key
  indiv_prod_data$flag = "Actual"
  
  tsData = ts(indiv_prod_data$n_tickets, frequency = 7)
  
  # SES
  ses_fcst = ses(tsData,h = forecasting_period,
                      alpha = 0.9,
                      initial = "simple",
                      exponential=TRUE)$mean
  
  ses_fcst = ceiling(as.vector(ses_fcst))
  
  arima.fit <- auto.arima(tsData)
  arima_fcst <- forecast(arima.fit, h= forecasting_period)$mean
  arima_fcst = ceiling(as.vector(arima_fcst))
  
  # Simple Moving Average
  
  sma_fcst = tryCatch(sma(tsData, h = forecasting_period)$forecast, error=function(err) ses(tsData,h = forecasting_period,
                                                                                                 alpha = 0.9,
                                                                                                 initial = "simple",
                                                                                                 exponential=TRUE)$mean)
  
  sma_fcst = ceiling(as.vector(sma_fcst))
  
  # combined fcst
  
  combined_dt = data.frame(key=rep(indiv_prod_data$key[1],forecasting_period))
  combined_dt$flag = "Forecast"
  combined_dt$channel_id = indiv_prod_data$channel_id[1]
  combined_dt$country_1 = indiv_prod_data$country_1[1]
  combined_dt$date = seq.Date(max(indiv_prod_data$date)+1,by = "day",length.out = forecasting_period)
  combined_dt$ses_fcst = ses_fcst
  combined_dt$arima_fcst = arima_fcst
  combined_dt$sma_fcst = sma_fcst
  combined_dt$n_tickets = ceiling((combined_dt$ses_fcst+combined_dt$arima_fcst+combined_dt$sma_fcst)/3)
  
  combined_dt$arima_fcst = NULL
  combined_dt$ses_fcst = NULL
  combined_dt$sma_fcst = NULL
  
  combined_dt = data.frame(combined_dt)
  hist_data = data.frame(indiv_prod_data[,names(combined_dt)])
  
  ts_fcst_output = rbind(ts_fcst_output,hist_data,combined_dt)
  
  rm(combined_dt,arima.fit,arima_fcst,sma_fcst,ses_fcst,indiv_prod_data)
  cat("COMPLETED FOR : ",list_for_ts[prod_ID],"\n")
}

write_csv(ts_fcst_output,"./output/ml_ts_fcst_output.csv")


colnames(rf_fcst_output)[which(names(rf_fcst_output)=="n_tickets")]="rf_n_tickets"
colnames(catboost_fcst_output)[which(names(catboost_fcst_output)=="n_tickets")]="cb_n_tickets"

ts_fcst_output$rf_n_tickets = ts_fcst_output$n_tickets
colnames(ts_fcst_output)[which(names(ts_fcst_output)=="n_tickets")]="cb_n_tickets"

combined = left_join(rf_fcst_output,catboost_fcst_output[,c("key","date","cb_n_tickets")])

final_fcst_output = rbind(ts_fcst_output,combined[,names(ts_fcst_output)])

write_csv(final_fcst_output,"./output/ml_fcst_output.csv")
