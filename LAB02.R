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
hpi.win=window(hpi.ts,start=2012+.25,end=2015)
## Lets apply the function that we created in earlier step once again. 
hpi.win=tsScale(hpi.win)
## Lets do the lag of the data (correlogram) to see if it is a good candidate for time series forecast. 
lag.plot(hpi.win,lag=9,do.lines=FALSE)
## lets install package forecast to further dig into ACF. 
install.packages("forecast")
library("forecast")
## lets call acf function that prints out ACF chart. 
acf(hpi.win)
## it seems that ACF is very strong for this data. 
## lets plot seasonal plots and monthly plots
seasonplot(hpi.win,year.labels=TRUE, year.labels.left = TRUE,col=1:3)
monthplot(hpi.win)
## Lets cut a training and test datasets. 
hpi.test=window(hpi.win,start=2014+.5)
hpi.train=window(hpi.win,end=2014+.45)
## Lets run the basic methods that we have learnt so far. 
## Lets run the average method where mean of the population is future. 
hpi.meanf=meanf(hpi.train,h=12)
## examine the result with 80% and 95% confidence interval. 
hpi.meanf 
## lets run naive method (last value is the future value)
hpi.naive=naive(hpi.train,h=12)
## see the output. 
hpi.naive
## lets run the snaive method (last seasonal value is future value)
hpi.snaive=snaive(hpi.train,h=12)
## see the output.
hpi.snaive
## Do the same with drift method
hpi.drift=rwf(hpi.train,h=12,drift=TRUE)
## lets plot all of these. 
plot(hpi.meanf,plot.conf=FALSE)
plot(hpi.naive,plot.conf=FALSE)
plot(hpi.snaive,plot.conf=FALSE)
plot(hpi.drift,plot.conf=FALSE)

## combine all these in one plot. We choose mean because we are not interested in plotting confidence intervals at this time. 

plot(hpi.win,main="House price index projection for next 12 months",ylab="house price index",xlab="years",xlim=c(2012.0,2016.5),ylim=c(100,135))
lines(hpi.meanf$mean,col=2)
lines(hpi.naive$mean,col=3)
lines(hpi.snaive$mean,col=4)
lines(hpi.drift$mean,col=5)
legend("bottomright", lty=1, col=c(2,3,4,5), cex=0.65,
       legend=c("Mean method","Naive method","Seasonal naive method","Drift Method"))
	   
## Lets see which one is the best one. Looking at the graph it seems that naive is the best. 

accuracy(hpi.meanf,hpi.test)
accuracy(hpi.drift,hpi.test)
accuracy(hpi.snaive,hpi.test)
accuracy(hpi.naive,hpi.test)

## our guess about naive has been validated. 

## Lets do some analysis of the residual.  If our work is good, we should suck out all ACF from the residual. 

res.meanf=residuals(hpi.meanf)
res.snaive=residuals(hpi.snaive)
res.naive=residuals(hpi.naive)
res.drift=residuals(hpi.drift)

## lets plot the acf of these residual. 
acf(na.omit(res.drift))
acf(na.omit(res.snaive))
acf(na.omit(res.naive))
acf(na.omit(res.meanf))

## Lets plot histogram to see if we see some normal distribution.  

hist(res.drift)
hist(res.snaive)
hist(res.naive)
hist(res.meanf)

## Last but not the least,we need to calculate Box tests (Pierce & Ljung)
## Pierce Method
Box.test(na.omit(res.snaive),lag=10,fitdf=0)  
## Ljung Method
Box.test(na.omit(res.snaive),lag=10,fitdf=0,type="L")
## Pierce Method
Box.test(na.omit(res.naive),lag=10,fitdf=0)  
## Ljung Method
Box.test(na.omit(res.naive),lag=10,fitdf=0,type="L")
## Pierce Method
Box.test(na.omit(res.meanf),lag=10,fitdf=0)  
## Ljung Method
Box.test(na.omit(res.meanf),lag=10,fitdf=0,type="L")
## Pierce Method
Box.test(na.omit(res.drift),lag=10,fitdf=0)  
## Ljung Method
Box.test(na.omit(res.drift),lag=10,fitdf=0,type="L")


## So these methods are not doing good and we need to explore new techniques. 
## Lets try this technique that we will cover in next classes. 
 fit=HoltWinters(hpi.win)
 res=residuals(fit)
 acf(res)
 Box.test(res,lag=10,fitdf=0,type="L")
plot(fit)