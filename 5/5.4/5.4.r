library(xts)
library(ggplot2)
library(scales)

gc<-read.csv(file="GC001.csv",header=TRUE)
names(gc)<-c('date','open','high','low','close','value','volumn')

gcx<-xts(gc[,-1],order.by=as.Date(gc$date,formate='%Y-%m-%d'))

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(gcx[,c('high','close')],melt=TRUE))
g<-g+geom_line(size=1)
g<-g+geom_hline(yintercept=10,col='blue',size=1)
g<-g+scale_y_continuous(breaks=seq(0,100,10))
g

library(lubridate)
gcx6<-gcx[which(gcx$high>6),]
table(wday(index(gcx6))-1)
table(days_in_month(index(gcx6)))

