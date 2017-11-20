install.packages(c('quantmod','PerformanceAnalytics'))
library(quantmod)
library(PerformanceAnalytics)
VANKE<-getSymbols("000002.SZ",auto.assign = FALSE, from = '2010-10-10')
close<-VANKE$'000002.SZ.Close'
ret<-CalculateReturns(close, method = "discrete")
cumret<-cumprod((ret+1)[-1])-1
VANKE_ret<-merge(close,ret,cumret)
names(VANKE_ret)<-c('close','ret','cumret')
print(tail(VANKE_ret))