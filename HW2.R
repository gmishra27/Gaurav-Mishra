getwd()
setwd("F:/LUC Study Material/ISSCM 495")
hpind=read.csv("hpi.csv",sep="|")
hpind.boston=hpind[c("YEAR","MA.Boston")]
install.packages("stringr")
library(stringr)
hpind.boston$YEAR=str_replace(str_replace(hpind.boston$YEAR," ",""),"^","01")
hpind.boston$YEAR=as.Date(hpind.boston$YEAR,"%d%B%Y")
plot(hpind.boston,type='l')
DataframeTS = function(x,y) {
  styr=as.numeric(format((head(x[1],1)),"%Y"))
  stmth=as.numeric(format((head(x[1],1)),"%m"))
  endyr=as.numeric(format((tail(x[1],1)),"%Y"))
  endmth=as.numeric(format((tail(x[1],1)),"%m"))
  return(ts(x[2],start=c(styr,stmth),end=c(endyr,endmth),frequency=y))
}
hpind.ts=DataframeTS(hpind.boston,12)
plot.ts(hpind.ts,type='l')
tsScale=function(x){
  fe=x[1]
  return((x/fe)*100)
}
hpind.win=window(hpind.ts,start=2008,end=2015)
hpind.win=tsScale(hpind.win)
plot(hpind.win)
hpind.win=window(hpind.ts,start=2012+.25,end=2015)
hpind.win=tsScale(hpind.win)
lag.plot(hpind.win,lag=9,do.lines=FALSE)
install.packages("forecast")
library("forecast")
acf(hpind.win)
seasonplot(hpind.win,year.labels=TRUE, year.labels.left = TRUE,col=1:3)
monthplot(hpind.win)
hpind.test=window(hpind.win,start=2014+.5)
hpind.train=window(hpind.win,end=2014+.45)
hpind.meanf=meanf(hpind.train,h=12)
hpind.meanf 
hpind.naive=naive(hpind.train,h=12)
hpind.naive
hpind.snaive=snaive(hpind.train,h=12)
hpind.snaive
hpind.drift=rwf(hpind.train,h=12,drift=TRUE)
hpind.drift

plot(hpind.meanf,plot.conf=FALSE)
plot(hpind.naive,plot.conf=FALSE)
plot(hpind.snaive,plot.conf=FALSE)
plot(hpind.drift,plot.conf=FALSE)

plot(hpind.win,main="House price index projection for next 12 months",ylab="house price index",xlab="years",xlim=c(2012.0,2016.5),ylim=c(100,135))
lines(hpind.meanf$mean,col=2)
lines(hpind.naive$mean,col=3)
lines(hpind.snaive$mean,col=4)
lines(hpind.drift$mean,col=5)
legend("bottomright", lty=1, col=c(2,3,4,5), cex=0.65,
       legend=c("Mean method","Naive method","Seasonal naive method","Drift Method"))

res.meanf=residuals(hpind.meanf)
res.snaive=residuals(hpind.snaive)
res.naive=residuals(hpind.naive)
res.drift=residuals(hpind.drift)


acf(na.omit(res.drift))
acf(na.omit(res.snaive))
acf(na.omit(res.naive))
acf(na.omit(res.meanf))

Box.test(na.omit(res.snaive),lag=10,fitdf=0)  
Box.test(na.omit(res.snaive),lag=10,fitdf=0,type="L")
Box.test(na.omit(res.naive),lag=10,fitdf=0)  
Box.test(na.omit(res.naive),lag=10,fitdf=0,type="L")
Box.test(na.omit(res.meanf),lag=10,fitdf=0)  
Box.test(na.omit(res.meanf),lag=10,fitdf=0,type="L")
Box.test(na.omit(res.drift),lag=10,fitdf=0)  
Box.test(na.omit(res.drift),lag=10,fitdf=0,type="L")

fit=HoltWinters(hpind.win)
res=residuals(fit)
acf(res)
Box.test(res,lag=10,fitdf=0,type="L")
plot(fit)

