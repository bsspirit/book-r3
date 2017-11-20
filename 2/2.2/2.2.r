library(zoo)
library(xts)

options(stringsAsFactors = FALSE)
dat<-read.csv(file="zn.csv",sep=",")
df<-xts(dat[,-1],order.by=as.POSIXct(dat[,1]))

head(df)

x<-as.numeric(df[,1])
y<-as.numeric(df[,2])

plot(y~x+1)

Xm<-mean(x);Xm
Ym<-mean(y);Ym

b <- sum((x-Xm)*(y-Ym)) / sum((x-Xm)^2) ;b
a <- Ym - b * Xm;b

lm.ab<-lm(y ~ 1+x)
lm.ab

plot(y~x+1)
abline(lm.ab,col='red')

summary(lm.ab)

y.res<-residuals(lm.ab)
head(y.res)
shapiro.test(y.res)
plot(y.res)

plot(lm.ab) 

par(mfrow=c(2,2))
plot(lm.ab)

df[c(27,192),]

df2<-df[-c(27,192),]
x2<-as.numeric(df2[,1])
y2<-as.numeric(df2[,2])
lm.ab2<-lm(y2 ~ 1+x2)
summary(lm.ab2)

newX<-data.frame(x=14040)
lm.pred<-predict(lm.ab,newX,interval="prediction",level=0.95)
lm.pred

plot(y~x+1)
abline(lm.ab,col='red')
points(rep(newX$x,3),y=lm.pred,pch=19,col=c('red','blue','green'))


