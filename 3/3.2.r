library(data.table)

df<-data.frame(a=c('A','B','C','A','A','B'),b=rnorm(6))
df

dt = data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt

class(df)
class(dt)

data.table

df<-data.frame(a=c('A','B','C','A','A','B'),b=rnorm(6))
class(df)
df2<-data.table(df)
class(df2)

dt <- data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
class(dt)
dt2<-data.frame(dt)
class(dt2)

dt = data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
dt[2,]
dt[2]
dt$a
dt[a=="B",]
dt[,a=='B']
which(dt[,a=='B'])
setkey(dt,a)
dt
dt["B",]
dt["B",mult="first"]
dt["B",mult="last"]
dt["b"]

dt = data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
dt[,c:=b+2]
dt
dt[,`:=`(c1 = 1:6, c2 = 2:7)]
dt
dt[,c('d1','d2'):=list(1:6,2:7)]
dt
dt[,c1:=NULL]
dt
dt[,c('d1','d2'):=NULL]
dt

dt[,b:=30]
dt
dt[a=='B' & c2>3, b:=100]
dt
dt[,b:=ifelse(a=='B' & c2>3,50,b)]
dt

dt = data.table(a=c('A','B','C','A','A','B'),b=rnorm(6))
dt
dt[,sum(b)]
dt[,sum(b),by=a]

student <- data.table(id=1:6,name=c('Dan','Mike','Ann','Yang','Li','Kate'));student
score <- data.table(id=1:12,stuId=rep(1:6,2),score=runif(12,60,99),class=c(rep('A',6),rep('B',6)));score
setkey(score,"stuId")
setkey(student,"id")
student[score,nomatch=NA,mult="all"]

rm(list=ls())
size = ceiling(1e8/26^2)
t0=system.time(
  df <- data.frame(x=rep(LETTERS,each=26*size),y=rep(letters,each=size))
)
t0
nrow(df)
object.size(df)
t1=system.time(
  val1 <- df[df$x=="R" & df$y=="h",]
)
t1

rm(list=ls())
size = ceiling(1e8/26^2)
t2=system.time(
  dt <- data.table(x=rep(LETTERS,each=26*size),y=rep(letters,each=size))
)
t2
nrow(dt)
object.size(dt)
t3=system.time(
val2 <- dt[x=="R" & y=="h",]
)
t3

setkey(dt,x,y)
t4=system.time(
  val3  <- dt[list("R","h")]
)
t4

size = 1000000
df <- data.frame(x=rep(LETTERS,each=size),y=rnorm(26*size))
system.time(
   df$y[which(df$x=='R')]<-10
)

dt <- data.table(x=rep(LETTERS,each=size),y=rnorm(26*size))
system.time(
   dt[x=='R', y:=10]
)
setkey(dt,x)
system.time(
   dt['R', y:=10]
)

size = 1000000*5
df <- data.frame(x=rep(LETTERS,each=size),y=rnorm(26*size))
system.time(
   df$y[which(df$x=='R')]<-10
)
rm(list=ls())
size = 1000000*5
dt <- data.table(x=rep(LETTERS,each=size),y=rnorm(26*size))
setkey(dt,x)
system.time(
   dt['R', y:=10]
)

size = 100000
dt <- data.table(x=rep(LETTERS,each=size),y=rnorm(26*size))
setkey(dt,x)
system.time(
 r1<-dt[,sum(y),by=x]
)
system.time(
  r2<-tapply(dt$y,dt$x,sum)
)
object.size(dt)

size = 100000*10
dt <- data.table(x=rep(LETTERS,each=size),y=rnorm(26*size))
setkey(dt,x)
val3<-dt[list("R")]
system.time(
   r1<-dt[,sum(y),by=x]
)
system.time(
   r2<-tapply(dt$y,dt$x,sum)
)
object.size(dt)

size = 100000*10*5
dt <- data.table(x=rep(LETTERS,each=size),y=rnorm(26*size))
setkey(dt,x)
system.time(
   r1<-dt[,sum(y),by=x]
)
system.time(
   r2<-tapply(dt$y,dt$x,sum)
)
object.size(dt)

