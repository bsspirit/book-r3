library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)

options(stringsAsFactors = FALSE) 
symbols<-c("^GSPC","^N225","^HSI","^STI","000001.SS")
suppressWarnings(getSymbols(symbols,src = "yahoo",from="2012-01-01"))

df<-merge(GSPC$GSPC.Adjusted,HSI$HSI.Adjusted,N225$N225.Adjusted,STI$STI.Adjusted,`000001.SS`$`000001.SS.Adjusted`)
names(df)<-c("GSPC","HSI","N225","STI","SSE")

head(df)
tail(df)
class(df)

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(df,melt=TRUE))
g<-g+geom_line(size=1)
g<-g+scale_y_continuous(breaks = seq(1000,30000,4000))
g<-g+ggtitle("Gloabel Index")
g

ret_df<-Return.calculate(df, method="discrete")
chart.CumReturns(ret_df,legend.loc="topleft", main="Cumulative Daily Returns for Gloabel Index")

Return.annualized(ret_df)

MACD<-function(dt,n=30){
  names(dt)<-c('close')

  dat<-na.locf(dt)
  dat$ma<-SMA(dat$close,n)
  
  sig_buy<-which(dat$ma-dat$close>0)
  sig_Sell<-which(dat$ma-dat$close<0)
  sig_buy<-sig_buy[which(diff(sig_buy)>1)]
  sig_Sell<-sig_Sell[which(diff(sig_Sell)>1)]
  if(first(sig_Sell)<first(sig_buy)) sig_Sell<-sig_Sell[-1]
  if(last(sig_Sell)<last(sig_buy)) sig_buy<-sig_buy[-length(sig_buy)]
  
  trade_dat<-do.call(rbind.data.frame, apply(cbind(sig_buy,sig_Sell),1,function(row){
    dt[row[1]:row[2],]
  }))
  
  ret_trade<-Return.calculate(trade_dat, method="discrete")
  return(ret_trade)
}

macd_ret<-lapply(df, function(col) MACD(col,30))
 
t(do.call(rbind.data.frame, lapply(macd_ret,Return.annualized)))
