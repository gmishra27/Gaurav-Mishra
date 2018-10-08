# Lab 3
# from "FORECASTING METHODS INFS495-SPRING-2016" 
#
#############################################################
## Step 0: Getting started with R
## Download the file "hpi.csv" from Sakai and save them in a 
## folder LAB03. I will refer your location as yourdirectory 
#############################################################
##setwd("use destination on your PC or MAC")
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

## So lets pick a smaller slice that has consistent trend. 
hpi.win=window(hpi.ts,start=2012+.25,end=2015)

plot.ts(hpi.win)
## Lets try Simple exponential smoothing method 
##(it is a constant method and we know we have trends and seasonality but try it to see how well it does)
install.packages("forecast")
library(forecast)

fit1 <- ses(hpi.win, alpha = 0.1, initial = "simple", h = 3)
fit2 <- ses(hpi.win, alpha = 0.5, initial = "simple", h = 3)
fit3 <- ses(hpi.win, alpha = 0.9, h= 3)

plot(fit1, plot.conf=FALSE, ylab="home price index-standardized",
     xlab="Year", main="plot of hpi-chicago", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.1), expression(alpha == 0.5),
         expression(alpha == 0.9)),pch=1)

hpi.test=window(hpi.win,start=2014+.5)
hpi.train=window(hpi.win,end=2014+.45)

hpi.ses1=ses(hpi.train,alpha=0.1,initial="simple",h=6)
hpi.ses2=ses(hpi.train,alpha=0.5,initial="simple",h=6)
hpi.ses3=ses(hpi.train,alpha=0.9,initial="simple",h=6)


accuracy(hpi.ses1,hpi.test)
accuracy(hpi.ses2,hpi.test)
accuracy(hpi.ses3,hpi.test)

res.ses1=residuals(hpi.ses1)
res.ses2=residuals(hpi.ses2)
res.ses3=residuals(hpi.ses3)

acf(na.omit(res.ses1))
acf(na.omit(res.ses2))
acf(na.omit(res.ses3))

Box.test(na.omit(res.ses1),lag=6,fitdf=0,type="L")
Box.test(na.omit(res.ses2),lag=6,fitdf=0,type="L")
Box.test(na.omit(res.ses3),lag=6,fitdf=0,type="L")

## Lets talk about Holts Method to take care of trend in the data. 

fit1=holt(hpi.train, alpha = 0.8, beta = 0.2, initial = "simple", h = 6)
## should the trend shows exponential shape, exponential=true ** multiplicative model. 
fit2 =holt(hpi.train, alpha = 0.8, beta = 0.2, initial = "simple", exponential = TRUE, h = 6)
fit3 = holt(hpi.train, alpha = 0.8, beta = 0.2, damped = TRUE, initial = "optimal", h = 6)


plot(hpi.win, ylab="home price index-standardized Holts Method",
     xlab="Year", xlim=c(2012.0,2016.5),ylim=c(100,135), main="plot of hpi-chicago", col="black", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("bottomright",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(simple), expression(multipicative),
         expression(damped)),pch=1)

accuracy(fit1,hpi.test)
accuracy(fit2,hpi.test)
accuracy(fit3,hpi.test)

res.fit1=residuals(fit1)
res.fit2=residuals(fit2)
res.fit3=residuals(fit3)

acf(na.omit(res.fit1))
acf(na.omit(res.fit2))
acf(na.omit(res.fit3))

Box.test(na.omit(res.fit1),lag=6,fitdf=0,type="L")
Box.test(na.omit(res.fit2),lag=6,fitdf=0,type="L")
Box.test(na.omit(res.fit3),lag=6,fitdf=0,type="L")

## But we know that data has seasonality and so lets explore Holts Winter Method. 

fit1 <- hw(hpi.train,seasonal="additive",h=6)
fit2 <- hw(hpi.train,seasonal="multiplicative",h=6)

plot(hpi.win, ylab="home price index-standardized Holts Method",
     xlab="Year", xlim=c(2012.0,2016.5),ylim=c(100,135), main="plot of hpi-chicago", col="black", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
legend("bottomright",lty=1, col=c(1,"blue","red","green"), 
       c("data", "multipicative",
         "additive"),pch=1)
		 
accuracy(fit1,hpi.test)
accuracy(fit2,hpi.test)

res.fit1=residuals(fit1)
res.fit2=residuals(fit2)

acf(na.omit(res.fit1))
acf(na.omit(res.fit2))

Box.test(na.omit(res.fit1),lag=6,fitdf=0,type="L")
Box.test(na.omit(res.fit2),lag=6,fitdf=0,type="L")
######################################################################


 
