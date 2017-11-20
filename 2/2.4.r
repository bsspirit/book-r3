set.seed(0)
x<-w<-rnorm(1000)
for(t in 2:1000) x[t]<-x[t-1]+w[t]
tsx<-ts(x)

head(tsx,15)
plot(tsx)

a<-ar(tsx);a
b<-ar(tsx,method = "ols");b
d<-ar(tsx,method = "mle");d

u<-mean(tsx)
v<-var(tsx)

p1<-sum((x[1:length(tsx)-1]-u)*(x[2:length(tsx)]-u))/((length(tsx)-1)*v);p1
p2<-sum((x[1:(length(tsx)-2)]-u)*(x[3:length(tsx)]-u))/((length(tsx)-1)*v);p2
p3<-sum((x[1:(length(tsx)-3)]-u)*(x[4:length(tsx)]-u))/((length(tsx)-1)*v);p3

acf(tsx)$acf
acf(tsx)

pacf(tsx)$acf
pacf(tsx)

predict(a,10,n.ahead=5L)
tsp<-predict(a,n.ahead=50L)

plot(tsx)
lines(tsp$pred,col='red')
lines(tsp$pred+tsp$se,col='blue')
lines(tsp$pred-tsp$se,col='blue')

library('forecast')
a2 <- arima(tsx, order=c(1,0,0))
tsp2<-forecast(a2, h=50)
plot(tsp2)
tsp2

