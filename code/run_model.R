
setwd("D:/project/FlixBus") # change working directory

library(readr)

# Enter number of forecasting period --------------------------------------------

forecasting_period = 10

# load and prepare data ---------------------------------------------------------
source("./code/data_preparation.R")

# forecast using time series model ----------------------------------------------
source("./code/ts_models.R") # takes 3-4 minutes

# forecast using machine learning model -----------------------------------------
source("./code/ml_models.R")

# generate final forecast -------------------------------------------------------

ts_forecast = read.csv("./output/ts_fcst_output.csv")
ml_forecast = read.csv("./output/ml_fcst_output.csv")
flixBus_fcst_output = left_join(ts_forecast,ml_forecast)
flixBus_fcst_output$key = NULL
flixBus_fcst_output$forecast = ceiling((flixBus_fcst_output$n_tickets+
                                          flixBus_fcst_output$cb_n_tickets)/2)

write_csv(flixBus_fcst_output, "./output/flixBus_fcst_output.csv")
