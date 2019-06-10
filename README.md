# H1 Machine Learning to Predict Bus Ticket Sales

In this repository, I have built a model to predict the sales of ticket on daily level. Given data has following variables

1. id	date	
2. channel_id	
3. country_1	
4. type	
5. n_tickets (number of tickets sold)	
6. weekday	
7. day_of_month	
8. isWeekend	
9. isWeekday	
10. month_end	
11. end_week_of_month	
12. start_week_of_month

### I have used following models: 

**Holt Winter Exponential Model**  
It’s is a way to model three aspects of the time series: a typical value (average), a slope (trend) over time, and seasonality
**Autoregressive Integrated Moving Average (ARIMA)**     
The AR part of ARIMA indicates that the variable of interest is regressed on its own lagged
The MA incorporates the dependency between an observation and a residual error from a moving average model applied to lagged observations.
**Simple Exponential Smoothing and Moving Average**  
These methods are suitable for forecasting data with no clear trend or seasonal pattern

**Random Forest**  
It’s is a ensemble model. It generates multiple small decision trees from random subsets of the data (hence the name “Random Forest”)
In case of regression, mean is used to generate prediction and for classification the majority vote is considered to classify a class
**CatBoost**  
CatBoost is an algorithm for gradient boosting on decision trees
It provides state of the art results and it is competitive with any leading machine learning algorithm on the performance front

#### For more details, please reach out to me at shilparai1407@gmail.com
