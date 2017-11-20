dfa<-read.csv(file="a.csv")
names(dfa)<-c("term","risk","ret","vol","fixed","cash","stock","alter","gains","loss")
head(dfa)

pairs(dfa)
library(PerformanceAnalytics)
chart.Correlation(dfa, histogram=TRUE, pch="+")

lv<-lm(loss~vol,data=dfa)
summary(lv)
plot(loss~vol,data=dfa)
abline(lv)

lr<-lm(loss~risk,data=dfa)
summary(lr)
par(mfrow=c(2,2))
plot(lr)

dfa2<-dfa[-30,]
lr2<-lm(loss~risk,data=dfa2)
summary(lr2)
plot(lr2)

dfb<-read.csv(file="b.csv",encoding="utf-8",fileEncoding = "utf-8")
names(dfb)<-c("name","code","create","type","first2014","first2015","first2016","last")
head(dfb)
paste(dfb$name,"(",dfb$code,")",sep="")

dfb$ret2014<-(dfb$first2015-dfb$first2014)/dfb$first2014
dfb$ret2015<-(dfb$first2016-dfb$first2015)/dfb$first2015
dfb$ret2016<-(dfb$last-dfb$first2016)/dfb$first2016
dfb$ret2014[c(which(is.na(dfb$ret2014)),which(is.infinite(dfb$ret2014)))]<-0
dfb$ret2015[c(which(is.na(dfb$ret2015)),which(is.infinite(dfb$ret2015)))]<-0
dfb$ret2016[c(which(is.na(dfb$ret2016)),which(is.infinite(dfb$ret2016)))]<-0
dfb[which(dfb$code=='OF217004'),]$ret2014<-0.0452
dfb[which(dfb$code=='OF217004'),]$ret2015<-0.036
dfb[which(dfb$code=='OF217004'),]$ret2016<-0.0237

dfc<-read.csv(file="c.csv")
names(dfc)<-c("term","risk","type","code","weight")
head(dfc)

library(plyr)
library(reshape2)
r1<-reshape(g1, idvar=c("term","risk"),timevar = "type",direction = "wide")
names(r1)<-c("term","risk","alter","cash","fixed","stock")
r1<-data.frame(na.fill(r1,0))

plan1<-dfc[dfc$term==1 & dfc$risk==1,]
plan1m<-merge(plan1[,c("term","risk","code","type","weight")],dfb[,c("code","ret2014","ret2015","ret2016")],by="code")
plan1m$ret2014w<-plan1m$weight*plan1m$ret2014
plan1m$ret2015w<-plan1m$weight*plan1m$ret2015
plan1m$ret2016w<-plan1m$weight*plan1m$ret2016
plan1m

plan1r<-ddply(plan1m,.(term,risk),summarise,ret2016=sum(ret2016w),ret2015=sum(ret2015w),ret2014=sum(ret2014w))
plan1r$cumret<-sum(c(plan1r$ret2016,plan1r$ret2015,plan1r$ret2014))
plan1r

plan1rm<-ddply(plan1m,.(term,risk,type),summarise,ret2016=sum(ret2016w),ret2015=sum(ret2015w),ret2014=sum(ret2014w))
plan1rm

planAll<-merge(dfc[,c("term","risk","code","type","weight")],dfb[,c("code","ret2014","ret2015","ret2016")],by="code")
planAll$ret2014w<-planAll$weight*planAll$ret2014
planAll$ret2015w<-planAll$weight*planAll$ret2015
planAll$ret2016w<-planAll$weight*planAll$ret2016
planAll

resutltA1<-ddply(planAll,.(term,risk),summarise,ret2016=sum(ret2016w),ret2015=sum(ret2015w),ret2014=sum(ret2014w))
resutltA1$mean<-rowMeans(resutltA1[,c('ret2016','ret2015','ret2014')])
resutltA1

resutltA1M<-merge(dfa,resutltA1,by=c("term","risk"))
resutltA1M<-resutltA1M[,c("term","risk","ret","vol","ret2016","ret2015","ret2014","mean")]
resutltA1M$ret2016<-resutltA1M$ret2016*100
resutltA1M$ret2015<-resutltA1M$ret2015*100
resutltA1M$ret2014<-resutltA1M$ret2014*100
resutltA1M$mean<-resutltA1M$mean*100
resutltA1M

chart.Correlation(resutltA1M[,-c(1,2,4)], histogram=TRUE, pch="+")
