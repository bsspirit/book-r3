library(xts)
library(reshape2)
library(ggplot2)

options(stringsAsFactors = FALSE)
dat<-read.csv(file="future.csv",sep=",")
df<-xts(dat[,-1],order.by=as.POSIXct(dat[,1]))

head(df,20)

lm1<-lm(y~x1+x2+x3+x4,data=df)
lm1
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

par(mfrow=c(1,1))
dfp<-predict(lm1,interval="prediction")
head(dfp,10)
mdf<-merge(df$y,dfp)

draw<-function(df){
  df2<-data.frame(df)
  df2$id<-index(df2)
  df2$date<-index(df)
  df3<-melt(df2,id=c("id","date"))
  
  g<-ggplot(data=df3,aes_string(x='id',y='value',colour='variable'))
  g<-g+geom_line()
  g
}
draw(mdf)

pairs(as.data.frame(df))

lm2<-update(lm1, .~. -x2)
summary(lm2)

lm3<-update(lm1, .~. + x1*x2)
summary(lm3)

lm4<-update(lm3, .~. -x1-x2)
summary(lm4)

step(lm1)
step(lm3)


dailyData <- function(file) {
  df <- read.table(file = file, header = FALSE,sep = ',', na.strings = 'NULL')
  names(df)<-c('date','price')
  return(df)
}

toXts<-function(data,format='%Y-%m-%d %H:%M:%S'){
  df<-subset(data,select=-c(date))
  xts(df,order.by=strptime(data$date, format))       
}

x1<-toXts(dailyData(file='j_daily.csv'),'%Y%m%d') 
x2<-toXts(dailyData(file='jm_daily.csv'),'%Y%m%d') 
x3<-toXts(dailyData(file='i_daily.csv'),'%Y%m%d') 
x4<-toXts(dailyData(file='hc_daily.csv'),'%Y%m%d') 
y<-toXts(dailyData(file='rb_daily.csv'),'%Y%m%d') 

df<-na.omit(merge(x1,x2,x3,x4,y))
names(df)<-c('x1','x2','x3','x4','y')

lm9<-lm(y~x1+x2+x3+x4,data=df)
summary(lm9)
summary(df)
pairs(as.data.frame(df))

