library(plyr)
library(xts)
library(TTR)
library(ggplot2)
library(scales)

read<-function(file){ 
  df<-read.table(file=file,header=FALSE,sep = ",", na.strings = "NULL")
  names(df)<-c("code","date","Open","High","Low","Close","Volume")
  dl<-split(df[-1],df$code)                            
  
  lapply(dl,function(row){         
    xts(row[-1],order.by = as.Date(row$date))
  })
}

data<-read("stock.csv")
class(data)
head(names(data))
length(data)

dateArea<-function(sDate=Sys.Date()-365,eDate= Sys.Date(),before=0){
  if(class(sDate)=='character') sDate=as.Date(sDate)
  if(class(eDate)=='character') eDate=as.Date(eDate)  
  return(paste(sDate-before,eDate,sep="/"))
}
head(data[['000001.SZ']])

title<-'300104.SZ'
stock<-data[[title]]                          
sDate<-as.Date("2015-01-01")                  
eDate<-as.Date("2015-08-24")                  
cdata<-stock[dateArea(sDate,eDate,360)]$Close 
vdata<-stock[dateArea(sDate,eDate,360)]$Volume

names(cdata)<-"Value" 
tail(cdata)
tail(vdata)

drawLine<-function(cdata,titie="Stock",sDate=min(index(cdata)),eDate=max(index(cdata)),breaks="1 year"){
   if(sDate<min(index(cdata))) sDate=min(index(cdata))
   if(eDate>max(index(cdata))) eDate=max(index(cdata))  
   cdata<-na.omit(cdata)
     
   g<-ggplot(aes(x=Index, y=Value),data=fortify(cdata[,1],melt=TRUE))
   g<-g+geom_line()
   if(ncol(cdata)>1){
       g<-g+geom_line(aes(colour=Series),data=fortify(cdata[,-1],melt=TRUE))  
   }
   g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
   g<-g+ylim(min(cdata$Value), max(cdata$Value))
   g<-g+xlab("") + ylab("Price")+ggtitle(title)
   g
}
drawLine(cdata,title,sDate,eDate,'1 month')

minmax<-function(data,max=20,min=10){
  d1<-na.locf(data,fromLast=TRUE)
  d2<-merge(d1,min=runMin(d1,min),max=runMax(d1,max))
  return(d2[,-1])
}

ldata<-cbind(cdata,minmax(cdata))
drawLine(ldata,title,sDate,eDate,'1 month')

buyPoint<-function(ldata){   
  idx<-which(ldata$Value == ldata$max)
  return(ldata[idx,])                                  
}

buydata<-buyPoint(ldata)
buydata

drawPoint<-function(ldata,pdata,titie,sDate,eDate,breaks="1 year"){
   ldata<-na.omit(ldata)
   g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
   g<-g+geom_line()
   g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
     
   if(is.data.frame(pdata)){
     g<-g+geom_point(aes(x=Index,y=Value,colour=op),data=pdata,size=4)
   }else{
     g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=na.omit(fortify(pdata,melt=TRUE)),size=4)  
   }
   g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
   g<-g+xlab("") + ylab("Price")+ggtitle(title)
   g
}

drawPoint(ldata,buydata$Value,title,sDate,eDate,'1 month')

stopPoint<-function(ldata,buydata){  
   idx<-which(ldata$Value == ldata$min)
   idx<-idx[which(c(0,diff(idx))!=1)]
   selldata<-ldata[idx,] 
   idx2<-sapply(index(buydata),function(e){
     head(which(index(selldata)>e),1)
   })
   return(selldata[unique(idx2),])
} 

selldata<-stopPoint(ldata,buydata)
selldata

bsdata<-merge(buydata$Value,selldata$Value)
names(bsdata)<-c("buy","sell")
drawPoint(ldata,bsdata,title,sDate,eDate,'1 month')

signal<-function(buy, sell){
   selldf<-data.frame(sell,op=as.character(rep("S",nrow(sell))))
   buydf<-data.frame(buy,op=as.character(rep("B",nrow(buy))))
   sdata<-rbind(buydf,selldf) 
   sdata[order(as.Date(row.names(sdata))),]
}
sdata<-signal(buydata,selldata)                                   
sdata

trade<-function(sdata,capital=100000,fixMoney=10000){
   amount<-0
   cash<-capital
   ticks<-data.frame()
   for(i in 1:nrow(sdata)){
      row<-sdata[i,]
      if(row$op=='B'){
         if(cash<fixMoney){
            print(paste(row.names(row),"No enough cash"))
            next
         }
         amount0<-floor(fixMoney/row$Value)
         amount<-amount+amount0
         cash<-cash-amount0*row$Value
      }  
      if(row$op=='S'){
         cash<-cash+amount*row$Value
         amount<-0
      }     
      row$cash<-round(cash,2)
      row$amount<-amount
      row$asset<-round(cash+amount*row$Value,2)
      ticks<-rbind(ticks,row)
   }
   ticks$diff<-c(0,round(diff(ticks$asset),2))
   rise<-ticks[intersect(which(ticks$diff>0),which(ticks$op=='S')),]
   fall<-ticks[intersect(which(ticks$diff<0),which(ticks$op=='S')),]
   return(list(ticks=ticks,rise=rise,fall=fall))
}

result<-trade(sdata,100000,10000)  
result$ticks

sellPoint<-function(ldata,buydata){
   arr<-c()
   for(i in 1:nrow(buydata)){
     if(i>1){
         date<-index(buydata[i,])  
         last<-as.vector(buydata[i-1,]$Value)
         lst<-ldata[paste(date,"/",sep="")]$Value      
         idx<-head(which(lst < last),1)
         if(length(idx)>0){        
             arr<-rbind(arr,index(lst[idx]))
         }
     }
   }
   selldata<-ldata[as.Date(unique(arr)),]
   bsdata<-merge(buydata$Value,selldata$Value)
   names(bsdata)<-c("buy","Value")
   idx1<-which(!is.na(bsdata$Value))
   idx2<-idx1[which(c(0,diff(idx1))==1)]
   bsdata$Value[idx2]<-NA
   return(bsdata$Value[which(!is.na(bsdata$Value))])
}

selldata<-sellPoint(ldata,buydata)
selldata

sdata<-signal(buydata$Value,selldata$Value)
sdata

stopdata<-stopPoint(ldata,buydata)

bsdata<-merge(buydata$Value,selldata$Value,stopdata$Value)
names(bsdata)<-c("buy","sell","stop")
drawPoint(ldata,bsdata,title,sDate,eDate,'1 month')
