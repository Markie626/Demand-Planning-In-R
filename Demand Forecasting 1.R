library(data.table)

install.packages("ggplot")
library(ggplot)


#read data
dt=fread("C:/Users/USER/Downloads/Demand Data.csv")
View(dt)

# Format Date, add additional columns
dt$Date = as.Date(dt$Date, "yyyy-mm-dd")
dt[, WeekDay := wday(Date) ]
dt[, WeekNum := week(Date) ]
dt[, Month   := month(Date)]
dt[, Year    := year(Date) ]

View(dt)

# Plot demand data for a chosen product
ggplot(data = dt, aes(x = Date, y = Paint)) +
  geom_line(color = "blue")+
  xlab("Year") +
  ylab("Daily Demand for Paint")

# Summarise demand by week and by month
cnames = names(dt)[2:7]

dtw = dt[, lapply(.SD, sum, na.rm=TRUE), 
         by = .(WeekNum,Year), .SDcols=cnames ]

dtm = dt[, lapply(.SD, sum, na.rm=TRUE), 
         by = .(Month,Year), .SDcols=cnames ]

head(dtw)
head(dtm)



# Filtering Data Tables
dtw[Year == 2020]
dtm[Year >= 2019]

# Extracting Columns as Vectors
dtm[, Paint]
dtm[Year >= 2020, Paint]

#------Time Series Objects (Examples) ---------------

x = sample(10:20,100, replace = TRUE)

ts(x, frequency = 12, start = c(2014,2))

ts(x, frequency = 4, start = c(2014,3))

ts(x, frequency = 4, 
   start = c(2014,3), end = c(2020,4))


#-----Create Time Series Objects From Demand Data ----

ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))

autoplot(ts_dtm) + 
  xlab("Year") +
  ylab("Monthly Demand for Paint")

ts_dtw = ts(dtw[,GardEquip], frequency = 52, start = c(2018,1))

autoplot(ts_dtw) + 
  xlab("Year") +
  ylab("Weekly Demand for Garden Equipment")

# Check If It's Time Series Object
is.ts(dt)  
is.ts(ts_dtw)


# ---------------------------------------------------------
# Task 2 - Analyse demand trend & seasonality 
# ---------------------------------------------------------

ts_dtw = ts(dtw[,Compost], frequency =52, start = c(2018,1))
autoplot(ts_dtw)

ts_dtm = ts(dtm[,DoorLock], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

# ------ACF and PACF --------------------------------------
#   
#   ACF  = Auto-correlation Function 
#   PACF = Partial Auto-correlation Function 

Acf(ts_dtm)
Pacf(ts_dtm)


# ------Decompose Data ------------------------------------

ts_dtm  %>%
  decompose() %>%
  autoplot()

ts_dtw  %>%
  decompose() %>%
  autoplot()


# ------Extract Decomposed Data ---------------------------

decompose(ts_dtm)$trend
decompose(ts_dtm)$seasonal
decompose(ts_dtm)$random

autoplot(ts_dtm - decompose(ts_dtm)$seasonal)
autoplot(ts_dtm - decompose(ts_dtm)$trend)

Pacf(ts_dtm - decompose(ts_dtm)$seasonal)


# ---------------------------------------------------------
# Practice - Analyse demand trend & seasonality 
# ---------------------------------------------------------

# Analyse QUARTERLY trend and seasonality for "GardEquip"
# Use demand from 2019 onwards, exclude 2018 data
# You will need a new table, "dtq" and new time series object
# DO NOT use "ts_dtw" and "ts_dtm" to define time series objects
# Use the function "quarter" to create additional column in dt
dt[,Quarter :=quarter(Date)]
View(dt)

dtq=dt[, lapply(.SD, sum, na.rm=TRUE), 
       by = .(Quarter,Year), .SDcols=cnames ]
head(dtq)

dtq[Year >= 2019,GardEquip]

ts_dtq = ts(dtq[,GardEquip], frequency =4, start = c(2019,1),end = c(2022,4))
autoplot(ts_dtq)

ts_dtq  %>%
  decompose() %>%
  autoplot()





# ---------------------------------------------------------
# Task 3 - Auto ARIMA modelling & parameters
# ---------------------------------------------------------

# ------What is ARIMA ? -----------------------------------
# AR     = Auto Regression - Regression on past values
# MA     = Moving Average model - Regression on past "errors"
# ARIMA  = Auto Regressive Integrated Moving Average

# AR - regression model - current values are 
#      linearly dependent on past values

# MR - regression model - current values are 
#      linearly dependent on past "errors" 

# Resource - https://otexts.com/fpp2/seasonal-arima.html


auto.arima(ts_dtm)

# ARIMA (p,d,q) (P,D,Q) [freq]
# (p,d,q) - non-seasonal part
# (P,D,Q) - seasonal part

# p,P - order (number of time lags) of the autoregressive model
# d,P - degree of first differencing 
# q,P - order of the moving-average model

# AIC = Akaike Information Criterion
# BIC = Bayesian Information Criterion

# AIC and BIC estimate prediction error
#     and quantity the information loss
#     the lesser the better model we have

# List the models using 'trace = TRUE'
auto.arima(ts_dtm, ic = "bic", trace = TRUE)

# "stepwise = FALSE" searches more models. It's slow.
auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

# The model can be stored in a variable
m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

# ---------------------------------------------------------
# Task 4 - Create and evaluate ARIMA demand model
# ---------------------------------------------------------

ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

m = Arima(ts_dtm, order = c(1,0,0),
          seasonal=list(order=c(0,1,0), period=12),
          include.drift = TRUE)
print(m)

Pacf(ts_dtm)

summary(m)

checkresiduals(m) # residuals normal, ACF matches periodicity



# ---------------------------------------------------------
# Task 5 - Forecast demand, plot, extact and evaluate.
# ---------------------------------------------------------

f = forecast(m, h = 12) # h = number of periods forecasted 
autoplot(f)

autoplot(f, include = 24) # previous data points to include

#----- Forecast Diagnosis  --------------------------------

checkresiduals(f)

plot(f$residuals)

qqnorm(f$residuals)
qqline(f$residuals, col = 2)

summary(f)

Acf(f$residuals)
Pacf(f$residuals)

#----- Write Output  -------------------------------------

out = as.data.table(f)
out = round(out,0)       # round the values to integers
write.csv(out,"Forecast.csv")
View(out)
# ---------------------------------------------------------
# Capstone Task - 52 Week Forecast for Garden Equipment

dtw[ ,GardEquip]

ts_dtw = ts(dtw[,GardEquip], frequency =52, start = c(2018,1),end = c(2022,4))
autoplot(ts_dtw)

ts_dtw  %>%
  decompose() %>%
  autoplot()

#model

n = auto.arima(ts_dtw, ic = "bic", trace = TRUE)
print(n)

auto.arima(ts_dtw, stepwise = FALSE, trace = TRUE)

o = Arima(ts_dtw, order = c(4,0,0),
          seasonal=list(order=c(1,1,0), period=52),
          include.drift = TRUE)
print(o)

Pacf(ts_dtw)

summary(o)

checkresiduals(o) # residuals normal, ACF matches periodicity


# ---------------------------------------------------------
# End of Guided Project - Thank You
# ---------------------------------------------------------
