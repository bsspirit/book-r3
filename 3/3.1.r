x<-matrix(1:12,ncol=3);x
apply(x,1,sum)

x <- cbind(x1=3, x2 = c(4:1, 2:5)); x
myFUN<- function(x, c1, c2) {
  c(sum(x[c1],1), mean(x[c2])) 
}
t(apply(x,1,myFUN,c1='x1',c2=c('x1','x2')))

df<-data.frame()
for(i in 1:nrow(x)){
  row<-x[i,]
  df<-rbind(df,rbind(c(sum(row[1],1), mean(row))))
}
df

data.frame(x1=x[,1]+1,x2=rowMeans(x))

rm(list=ls())
fun1<-function(x){
  myFUN<- function(x, c1, c2) {
    c(sum(x[c1],1), mean(x[c2])) 
  }
  apply(x,1,myFUN,c1='x1',c2=c('x1','x2'))
}

fun2<-function(x){
  df<-data.frame()
  for(i in 1:nrow(x)){
    row<-x[i,]
    df<-rbind(df,rbind(c(sum(row[1],1), mean(row))))
  }
}

fun3<-function(x){
  data.frame(x1=x[,1]+1,x2=rowMeans(x))
}

x <- cbind(x1=3, x2 = c(400:1, 2:500))
system.time(fun1(x))
system.time(fun2(x))
system.time(fun3(x))

x <- list(a = 1:10, b = rnorm(6,10,5), c = c(TRUE,FALSE,FALSE,TRUE));x
lapply(x,fivenum)

x <- cbind(x1=3, x2=c(2:1,4:5))
x; class(x)
lapply(x, sum)
lapply(data.frame(x), sum)

x <- cbind(x1=3, x2=c(2:1,4:5))
sapply(x, sum)
sapply(data.frame(x), sum)

class(lapply(x, sum))
class(sapply(x, sum))

lapply(data.frame(x), sum)
sapply(data.frame(x), sum, simplify=FALSE, USE.NAMES=FALSE)

a<-1:2
sapply(a,function(x) matrix(x,2,2))
sapply(a,function(x) matrix(x,2,2), simplify='array')

val<-head(letters)
sapply(val,paste,USE.NAMES=FALSE)
sapply(val,paste,USE.NAMES=TRUE)

x <- data.frame(cbind(x1=3, x2=c(2:1,4:5)))
vapply(x,cumsum,FUN.VALUE=c('a'=0,'b'=0,'c'=0,'d'=0))

a<-sapply(x,cumsum)
row.names(a)<-c('a','b','c','d')

set.seed(1)
x<-1:10;x
y<-5:-4;y
z<-round(runif(10,-5,5));z
mapply(max,x,y,z)

set.seed(1)
n<-rep(4,4)
m<-v<-c(1,10,100,1000)
mapply(rnorm,n,m,v)

tapply(iris$Petal.Length,iris$Species,mean)

set.seed(1)
x<-y<-1:10;x;y
t<-round(runif(10,1,100)%%2);t
tapply(x,t,sum)
tapply(x,t,sum,y)

set.seed(1)
x=list(a=12,b=4:1,c=c('b','a'))
y=pi
z=data.frame(a=rnorm(10),b=10:1)
a <- list(x=x,y=y,z=z)

rapply(a,sort, classes='numeric',how='replace')
class(a$z$b)
rapply(a, function(x) paste(x,'++++'), classes="character",deflt=NA, how="list")

env <- new.env(hash = FALSE) 
env$a <- 1:10
env$beta <- exp(-3:3)
env$logic <- c(TRUE, FALSE, FALSE, TRUE)
ls(env)
ls.str(env)
eapply(env, mean)
ls()
eapply(environment(), object.size)




