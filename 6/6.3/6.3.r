set.seed(1)
dates<-as.Date('2010-01-01')+1:100
x<-round(rnorm(100,50,40),2)
y<-round(rnorm(100,50,40),2)
df<-data.frame(dates,x,y)
head(df,20)

library(ggplot2)
library(scales)
library(reshape2)

df2<-melt(df,c('dates'))

g<-ggplot(data=df2,aes(x=dates,y=value,colour=variable))
g<-g+geom_line()
g<-g+scale_x_date(date_breaks = "1 week",date_labels='%m-%d')
g<-g+labs(x='date',y='Price')
g

df$diff<-df$x-df$y
idx<-which(df$diff>10)
idx<-idx[-which(diff(idx)==1)-1]

idx
head(df,20)

xprofit<- df$x[4]-df$x[6];xprofit
yprofit<- df$y[6]-df$y[4];yprofit

plot(df$diff,type='l')

library(xts)
library(TTR)

read<-function(file){ 
  df<-read.table(file=file,header=FALSE,sep = ",", na.strings = "NULL")
  names(df)<-c("date","Open","High","Low","Close") 
  dl<-split(df,format(as.POSIXct(df$date),'%Y-%m-%d')) 
  
  lapply(dl,function(item){
    xts(item[-1],order.by = as.POSIXct(item$date))
  })
}

cu1605<-read(file='cu1605.csv')
cu1606<-read(file='cu1606.csv')

class(cu1605) 
names(cu1605)
nrow(cu1605[[1]])
head(cu1605[['2016-02-01']])

xdf<-merge(cu1605[['2016-02-01']]$Close,cu1606[['2016-02-01']]$Close)
names(xdf)<-c('x1','x2')

xdf<-na.locf(xdf)
xdf$diff<-xdf$x1-xdf$x2
head(xdf,20)

range(xdf$diff)
mean(xdf$diff)
hist(xdf$diff,10)

library(devtools)
install_github('bsspirit/DanQuant')
library(DanQuant)

target.pair<-function(xdf){
  xdf$diff<-xdf$x1-xdf$x2
  xdf$mid<- -63  
  xdf$top<- -45  
  xdf$bottom<- -75
  return(xdf)
}

mod.pair<-function(tXTS,params){
  ops<-model.boll(tXTS)
  ops<-model.filter.pingLatest(ops)
  ops<-model.filter.pingStrat(ops)
  ops<-model.filter.closeOut(ops,tXTS,1)
  ops<-model.filter.stopProfit.pair(ops,tXTS,500,params)
  ops<-model.filter.posPeriod(ops,40)  
  ops<-model.filter.contineKai(ops) 
  ops<-model.filter.continePing(ops) 
  return(ops)
}

library(DanQuant)

params<-newParams('cu1605','cu1606','2016-02-03')
tXTS<-target.pair(xdf)
ops<-mod.pair(tXTS,params)
sigs<-backtest.signal(tXTS,ops)
report<-backtest.pair(sigs,params)
page<-backtest.reading.pair(report)
result<-backtest.summary(page)
drawReport.pair(tXTS,sigs,params)

days<-as.character(tDays('2016-02-01','2016-02-29'))
code1<-'cu1605'
code2<-'cu1606'
cu_cu<-batch.backtest.pair(days,code1,code2,target.pair,mod.pair)

