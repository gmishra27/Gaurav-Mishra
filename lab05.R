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
  return(ts(as.numeric(unlist(x[2])),start=c(styr,stmth),end=c(endyr,endmth),frequency=y))
}
## Use this function to create a time series.  
hpi.ts=DataframeTS(hpi.ss,12)

## lets plot it with line type= line. 
plot.ts(hpi.ts,type='l')

## How to see the trend using moving average which is 2X12 MA. If centre if false it will be only 12 MA
trend=ma(hpi.ts, order=12,centre=TRUE)
plot(trend)
lines(hpi.ts,col="green",lwd=2)
## How to do classical decomposition the time series using MA. 

fit=decompose(hpi.ts, type ="additive")
plot(fit)
## how to do decomposition using STL 

fit=stl(hpi.ts,t.window=3, s.window="periodic")

## How to forecast using STL
fcast=forecast(fit,method="arima",allow.multiplicative.trend=FALSE)
plot(fcast)
accuracy(fcast)

## How to estimate the Exponential smoothing model

fit=ets(hpi.ts)
fcast=forecast(fit,h=12)
plot(fcast)

## lets install vars
install.packages("vars")
library(vars)
## Read the data from the file. 
labor=read.csv("labordata.txt",sep="|")
## convert the data into time series.  It is a quarterly data so frequency =4
labor.ts= ts(labor,start=1980,frequency=4)
## Now we have to select the p and the k (k=4 because we have four variables) how to do that. 
Varselect(labor.ts,lag.max=10,type="const")
##calculate the model. 
var=VAR(labor.ts,p=1,type="const")
## lets see if we see any correlation in the residuals. The null hypothesis is that there is no serial correlation in the residuals. 
serial.test(var,lags.pt=10)
## Now we can forecast based on the model for next 12 months.
fcast=forecast(var,h=12)
## plot the model to see the forecast. 
plot(fcast)
