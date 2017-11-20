library(xts)
library(ggplot2)

bs<-read.csv("bondSaving.csv",header=TRUE)
names(bs)<-c("date","bond","saving")

bsxts<-xts(bs[-1],order.by = as.Date(bs$date))
bsxts<-na.locf(bsxts,fromLast=FALSE)

g<-ggplot(aes(x=Index, y=Value, colour=Series),data=fortify(bsxts,melt=TRUE))
g<-g+geom_line()
g 

bonds<-read.csv("bonds.csv",header=TRUE)
names(bonds)<-c("date","ytm1","ytm3","ytm10")

bondsxts<-xts(bonds[-1],order.by = as.Date(bonds$date))
bondsxts<-na.locf(bondsxts,fromLast=FALSE)

g<-ggplot(aes(x=Index, y=Value, colour=Series),data=fortify(bondsxts,melt=TRUE))
g<-g+geom_line()
g 

apply(bonds[,-1],2,sd)
sdy<-apply.yearly(bondsxts,function(cols) apply(cols,2,sd))

apply(sdy,2,max)
sdy[unique(apply(sdy,2,which.max)),]


library(lubridate)
library(reshape2)

ytm<-bondsxts$ytm10['2010/']
years<-unique(year(index(ytm)))
df<-data.frame(matrix(NA,ncol=length(years),nrow=366))
names(df)<-years

apply.yearly(ytm,function(x){
  ycol<-format(index(x),'%Y')[1]
  df[yday(x),ycol]<<-x
})

df<-cbind(id=1:366,df)
dt<-na.omit(melt(df,id.vars="id"))

g<-ggplot(aes(x=id,y=value, colour=variable),data=dt)
g<-g+geom_line(size=1)
g<-g+xlab('Day of Year')
g 


