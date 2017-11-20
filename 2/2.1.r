library(devtools)
install_github("joshuaulrich/quantmod")

library(quantmod) 
library(PerformanceAnalytics)

getSymbols(c('IBM','GE','^GSPC'), from = '2010-01-01')

head(GSPC)
tail(GSPC)
barChart(GSPC)

names(IBM)<-c("open","high","low","close","volume","adjusted")
names(GE)<-c("open","high","low","close","volume","adjusted")
names(GSPC)<-c("open","high","low","close","volume","adjusted")

dat=merge(IBM$adjusted,GE$adjusted,GSPC$adjusted)
names(dat)<-c('IBM','GE','SP500')
head(dat)

IBM_ret=dailyReturn(IBM)  
GE_ret=dailyReturn(GE)
SP500_ret=dailyReturn(GSPC)

dat_ret=merge(IBM_ret,GE_ret,SP500_ret)
names(dat_ret)<-c('IBM','GE','SP500')
head(dat_ret)

Rf<-0.04/12
results<-table.AnnualizedReturns(dat_ret,Rf=Rf);results
table.Stats(dat_ret)

chart.Bar(dat_ret[,1], main="IBM Daily Returns")
chart.Bar(monthlyReturn(IBM), main="IBM Monthly Returns")
chart.CumReturns(dat_ret,main="Total Returns",legend.loc="topleft")
chart.Correlation(dat_ret, histogram=TRUE, pch="+")

CAPM.alpha(dat_ret[,1:2],dat_ret[,3],Rf=Rf)
CAPM.beta(dat_ret[,1:2],dat_ret[,3],Rf=Rf)
