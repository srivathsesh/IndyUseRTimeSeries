# This code is for testing out the behavior of tsibble for 5 business days
# The benchamrk behavior is of a ts() object.
# Additionally the fable package is explored to see how the behavior compares to forecast package

#*****************************
#       Load Packages
#*****************************

library(fpp3)

# library(forecast)

#devtools::install_github("DavisVaughan/calendarrr")



# *****************************
#    Generating a ts object
# *****************************
set.seed(123)

# notice the frequency is set to 365.25, the reason is that the way we tell R, that the data is of daily.
# Why 365.25? It accounts for leap year


tsobj <- ts(rnorm(11),start = ymd("2020-06-01"),frequency = 365.25)

plot.ts(tsobj)

# ---------------------------------
# hate the way the plot shows up 
#-----------------------------------

# Ideally I want the business day to show up on the plot excluding weekends
# So creating the dates explicitly here 

days <- ymd("2020-06-01") + 0:14

# get business days based on US calendar

bdays <- days[calendarrr::cal_is_business_day(days)]


plot.ts(tsobj,axes=F) # don't plot the axes yet
axis(2) # plot the y axis
axis(1, labels=bdays, at=time(tsobj) )
box() # and the box around the plot

#********************************************
#     Plot days of the week as the season
#********************************************

forecast::seasonplot(tsobj,5,
                     season.labels = c("M","T","W","Th","F"),
                     col = c("red","purple","green"),
                     year.labels = T
                      )
# It is a pain to adjust the label for a custom season. Here week. The numbers are not interpretable

#***********************************************************
#    What if I made a ts object with custom frequency of 5?
#***********************************************************



tsobj2 <- ts(as.vector(tsobj),start = ymd("2020-06-01"),frequency = 5)

forecast::ggseasonplot(tsobj2)


# Again not super helpful


#------------------------------------------------------------------
#      the frequency change should not affect the ACF or PACF
#------------------------------------------------------------------


forecast::Acf(tsobj) -> p1
forecast::Acf(tsobj2) -> p2
forecast::Pacf(tsobj) -> p3
forecast::Pacf(tsobj2) -> p4



#------------------------------------
#      Seting up a tsibble object
#----------------------------------


tsbldata <- as_tsibble(data.frame(values = as.vector(tsobj), index = bdays),index = index)


tsbldata %>% 
  fill_gaps() %>% # fill with NA
  autoplot()


tsbldata %>% 
  fill_gaps() %>%
  gg_season(period = 'week',labels = "left")

# I like the seasonality plot here



#*******************************
#       Autocorrelations
#******************************

tsbldata %>% 
  fill_gaps() %>%
  gg_tsdisplay(lag_max = 10)

tsbldata %>% 
  fill_gaps() %>% 
  ACF(lag_max = 10) %>% 
  autoplot()

#------------------------------------------
#     Compare tsibble ACF with forecasts
#------------------------------------------

tsbldata %>% 
  fill_gaps() %>% 
  ACF(lag_max = 10)


p1


# Urggggg Why????????/


#******************************************************
#       Does the Autocorrelation difference
#           affect models - espically ARIMA
#******************************************************


forecast::auto.arima(tsobj)


tsbldata %>% 
  fill_gaps() %>% 
  model(arima = ARIMA()) %>% 
  report()


#--------------------------------
#        Perfect sign wave ?
#-------------------------------

x <- seq(0,8*pi,length.out=11)
y <- sin(x)
plot(x,y,type="l")


sints <- ts(y,frequency = 5)

plot.ts(sints,axes=F) # don't plot the axes yet
axis(2) # plot the y axis
axis(1, labels=bdays, at=time(sints) )
box() # and the box around the plot

sints_tsbl <- as_tsibble(data.frame(value = as.vector(sints), index = bdays))

sints_tsbl

forecast::auto.arima(sints)

forecast::auto.arima(sints) -> f1

plot(forecast::forecast(f1,h = 5))

sints_tsbl %>% 
  fill_gaps() %>% 
  model(arima = ARIMA()) -> f2

f2 %>% 
  forecast(h=5) %>% 
  autoplot(sints_tsbl)



# Conclusion

# The modeling with fill_gaps() affects ARIMA model.



