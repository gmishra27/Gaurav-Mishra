############################################################################################################################
##Lets do something that we have done before i.e. prep up the data, set the working directory etc. 
############################################################################################################################
## Set the working directory
setwd("your working directory based on MAC or PC")

##Read the file in a dataframe with pipe (|) delimiter. 
hpi=read.csv("hpi.csv",sep="|")

##lets subset the relevant data from the entire dataset. 
hpi.ss=hpi[c("YEAR","IL.Chicago")]

## convert the IL.chicago to the standard inflation adjustment transformation. 
## install package to do some manipulation with year column and remove space and append 01 to make it look like a date and not string. 

install.packages("stringr")
library(stringr)

## Remove the empty space between year and month and then put 01 in the front of the string. 
## ^ means starting of the string. 
hpi.ss$YEAR=str_replace(str_replace(hpi.ss$YEAR," ",""),"^","01")
## convert this string into a data by using as.Date function.  We need to give the format. %d stands for day
## %B stands for month in full form e.g. March. %Y stands for year in full form e.g. 2016. 
hpi.ss$YEAR=as.Date(hpi.ss$YEAR,"%d%B%Y")
## Lets plot the graph. 
plot(hpi.ss,type='l')

##However, hpi.ss is a dataframe and we need to convert it into a time series datatype. 
## We will create a function that takes the two inputs (dataframe like hpi.ss and frequency of the data)
## since our data is monthly, our frequency is 12.  If it was quarterly ...
## The function extracts start year, start month, end year, end month and use ts function to create a time series. 
DataframeTS = function(x,y) {
  styr=as.numeric(format((head(x[1],1)),"%Y"))
  stmth=as.numeric(format((head(x[1],1)),"%m"))
  endyr=as.numeric(format((tail(x[1],1)),"%Y"))
  endmth=as.numeric(format((tail(x[1],1)),"%m"))
  return(ts(x[2],start=c(styr,stmth),end=c(endyr,endmth),frequency=y))
}
## Use this function to create a time series.  
hpi.ts=DataframeTS(hpi.ss,12)

## lets plot it with line type= line. 
plot.ts(hpi.ts,type='l')

## Lets create a function to do adjustment to the series. 
## This function takes a time series as an input and divides the entire time series by its first value. 
tsScale=function(x){
  fe=x[1]
  return((x/fe)*100)
}
## Since we are interested in recent data (2008 onwards), lets subset the time series by using a function window. 
hpi.win=window(hpi.ts,start=2008,end=2015)
## apply the tsScale function to see data as percentage of 2008 level. 
hpi.win=tsScale(hpi.win)
## plot the data and see if it makes sense from forecasting purpose. 
plot(hpi.win)
## This may pose a problem from our mean or drift methods. We may get a forecast which is influenced by 2008 super high values. 
## So lets pick a smaller slice that has consistent trend. 
hpi.win=window(hpi.ts,start=2010+.25,end=2015)
## Lets apply the function that we created in earlier step once again. 
hpi.win=tsScale(hpi.win)
## Lets do the lag of the data (correlogram) to see if it is a good candidate for time series forecast. 
lag.plot(hpi.win,lag=9,do.lines=FALSE)
## lets install package forecast to further dig into ACF. 
install.packages("forecast")
library("forecast")
require(tseries)
## lets call acf function that prints out ACF chart. 
acf(hpi.win)
## it seems that ACF is very strong for this data. 
## lets plot seasonal plots and monthly plots
seasonplot(hpi.win,year.labels=TRUE, year.labels.left = TRUE,col=1:3)
monthplot(hpi.win)
## Lets cut a training and test datasets. 
hpi.test=window(hpi.win,start=2014+.5)
hpi.train=window(hpi.win,end=2014+.45)
############################################################################################################################
## idea of differencing 
############################################################################################################################

hpi.diff=diff(hpi.ts,1,2)
plot(hpi.diff)
hpi.diff=diff(hpi.ts,12,1)
plot(hpi.diff)
## lets do box cox tranformation and then do double differencing (seasonal with ordinary differencing)
hpi.diff=diff(diff(BoxCox(hpi.ts,lambda = BoxCox.lambda(hpi.ts)),12,1),1,2)
plot(hpi.diff)
## so seasonal differencing of 1 and double differencing on top of seasonal differencing makes the time series stationary. 
## use adf test and kpss test to confirm what you see. 
adf.test(hpi.diff,alternative="stationary")
## p value is less than 0.05 so we can reject null hypothesis and accept alternate hypothesis that hpi.diff is stationary. 
kpss.test(hpi.diff)
## p value is greater than 0.05 and hence we cannot reject null hypothesis that hpi.diff is stationary. 
############################################################################################################################
# Lets see our training and test dataset. 
############################################################################################################################
## look at ACF and PACF to find out if it is a MA(q) or AR(p) model. 
hpi.diff=diff(hpi.train,1,1)
kpss.test(hpi.diff)
adf.test(hpi.diff)
Acf(hpi.diff,main="acf",lag.max=72)
Pacf(hpi.diff,main="pacf")

fit=Arima(hpi.train, order=c(2,1,1))
## see the information of the model 
fit
res=residuals(fit)
## lets see if residuals are distributed normally. 
hist(res)

Box.test(res,lag=10,fitdf=3,type="L") 

############################################################################################################################
##lets do the forecasting for next 12 months
############################################################################################################################
fc=forecast(fit,h=12)
plot(fc)
lines(hpi.test,col="green")
accuracy(fc,hpi.test)
############################################################################################################################
##could I do this in a simpler way and auto estimate the p q and d. 
############################################################################################################################
fit_auto=auto.arima(hpi.train)
## lets see the model. 
fit_auto
#### it gives arima(2,1,0) as ideal model but with just in one step. 
fc_auto=forecast(fit_auto,h=12)
plot(fc_auto)
lines(hpi.test,col="green")
accuracy(fc_auto,hpi.test)
############################################################################################################################
##Lets take another example of airlines from mid-term.
############################################################################################################################
air=read.csv("a.csv",sep=",",header=TRUE)
air$Month=(str_replace(air$Month,"^","01-"))
air$Month=as.Date(air$Month, "%d-%Y-%m") 
plot(air, type='l')
DataframeTS = function(x,y) {
  styr=as.numeric(format((head(x[1],1)),"%Y"))
  stmth=as.numeric(format((head(x[1],1)),"%m"))
  endyr=as.numeric(format((tail(x[1],1)),"%Y"))
  endmth=as.numeric(format((tail(x[1],1)),"%m"))
  return(ts(x[2],start=c(styr,stmth),end=c(endyr,endmth),frequency=y))
}
air.ts=DataframeTS(air,12)
lag.plot(air.ts,lag=9,do.lines=FALSE)
library("forecast")
acf(air.ts)
seasonplot(air.ts,year.labels=TRUE, year.labels.left = TRUE,col=1:12)
monthplot(air.ts)
air.test=window(air.ts,start=1960)
air.train=window(air.ts,end=1960-.001)
air.train
air.test
air.diff=diff(air.train,1,1)
acf(air.diff)
## we see that ordinary differencing has not removed seasons.  So we need a seasonal differencing too. 
air.diff=diff(air.diff,12,1)
plot(air.diff)
acf(air.diff)
## lets check adf test to confirm that we have achieved stationarity. 
kpss.test(air.diff)
adf.test(air.diff)
## Now acf shows on seasonality. We also see that there 1 lag that is significant. Henceforth q=1. 
## lets use a function called tsdisplay to see acf and pacf in one plot and find out the order.
tsdisplay(air.diff)
## so ACF shows 1 lag and PACF shows 1 lag so q=1 and p=1 and we have d=1 and D=1
fit=Arima(air.train, order=c(1,1,1), seasonal=c(0,1,0))
fit
res=residuals(fit)
Box.test(res, lag=24, fitdf=2,type="L")
## p value is large so residuals are random. 
hist(res)
hist(res,breaks=20)
## lets do forecasting. 

fc=forecast(fit,h=12)
plot(fc)
lines(air.test,col="red")
accuracy(fc,air.test)

## Lets do the same with auto.arima function. 

fit_auto=auto.arima(air.train)
fit_auto
fc_auto=forecast(fit_auto,h=12)
plot(fc_auto)
lines(air.test,col="red")
accuracy(fc_auto,air.test)


############################################################################################################################
##Lets take another example of female unemployment from mid-term.
############################################################################################################################
uemp=read.csv("fn.csv",sep="|",header=TRUE)
summary(uemp)
##1948-01 is the format,  So we need to add  01 at the end. $ denotes the end of line in regex. 
uemp$Month=str_replace(uemp$Month,"$","-01")
## convert the string to date. 
uemp$Month=as.Date(uemp$Month, "%Y-%m-%d") 
## plot the data. 
plot(uemp,type='l')

uemp.ts=DataframeTS(uemp,12)

uemp.ts=tsScale(uemp.ts)
## lets create training and test datasets
uemp.train=window(uemp.ts,start=1975,end=1981-.001)
uemp.test=window(uemp.ts,start=1981)
## see how test and training datasets look.
uemp.train
uemp.test
## is this data stationary? 
adf.test(uemp.train)
## No, data is not stationary and p value is greater than 97%.  First try differencing too make the data stationary. 
uemp.diff=diff(uemp.train)
## lets do adf.test again
adf.test(uemp.diff)
kpss.test(uemp.diff)
## data is stationary now and I can use d=1 for the arima model. 
Acf(uemp.diff)
Pacf(uemp.diff)
## lets see the 72 lags to find out seasonal pattern in the ACF & PACF
Acf(uemp.diff,lag.max=72)
Pacf(uemp.diff,lag.max=72)
## We do see that it peaks at different seasons so lets do seasonal differencing. Remember order does not matter. 
uemp.diff=diff(uemp.diff,12,1)
## Now see the acf. 
Acf(uemp.diff,lag.max=72)
pacf(uemp.diff,lag.max=72)

Acf(uemp.diff,lag.max=5)
Pacf(uemp.diff,lag.max=5)
## so I see one lag at 12 months for ACF and PACF and 1 lag for ACF and PACF
fit=Arima(uemp.train, order=c(1,1,2),seasonal=c(1,1,1))
## check the residuals. 
res=residuals(fit)
plot(res)
hist(res)
## Check box ljung test. 
Box.test(res, lag=24, fitdf=6,type="L")
## P value is 40% so data is random. 
fc=forecast(fit,h=12)
plot(fc)
lines(uemp.test)
accuracy(fc,uemp.test)
## Now lets do the same for different models in +1 -1 of p,q, P and Q and see which gives lowest RMSE

fit=arima(uemp.train, order=c(1,1,0),seasonal=c(1,1,0))
## check the residuals. 
res=residuals(fit)
plot(res)
hist(res)
## Check box ljung test. 
Box.test(res, lag=12, fitdf=6,type="L")
## P value is 40% so data is random. 
fc=forecast(fit,h=12)
plot(fc)
lines(uemp.test)
accuracy(fc,uemp.test)

fit=arima(uemp.train, order=c(1,1,0),seasonal=c(2,1,0))
## check the residuals. 
res=residuals(fit)
plot(res)
hist(res)
## Check box ljung test. 
Box.test(res, lag=12, fitdf=5,type="L")
## P value is 40% so data is random. 
fc=forecast(fit,h=12)
plot(fc)
lines(uemp.test)
accuracy(fc,uemp.test)

fit=arima(uemp.train, order=c(1,1,0),seasonal=c(2,0,0))
## check the residuals. 
res=residuals(fit)
plot(res)
hist(res)
## Check box ljung test. 
Box.test(res, lag=12, fitdf=4,type="L")
## P value is 40% so data is random. 
fc=forecast(fit,h=12)
plot(fc)
lines(uemp.test)
accuracy(fc,uemp.test)

## or just go to auto.arima and tune around that model. 
fit=auto.arima(uemp.train)
fc=forecast(fit,h=12)
plot(fc)
lines(uemp.test)
accuracy(fc,uemp.test)