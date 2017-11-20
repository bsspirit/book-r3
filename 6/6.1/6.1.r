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
head(data[['000001.SZ']])

dateArea<-function(sDate=Sys.Date()-365,eDate= Sys.Date(),before=0){ 
    if(class(sDate)=='character') sDate=as.Date(sDate)
    if(class(eDate)=='character') eDate=as.Date(eDate)  
    return(paste(sDate-before,eDate,sep="/"))
}

ma<-function(cdata,mas=c(5,20,60)){
    if(nrow(cdata)<=max(mas)) return(NULL)
    ldata<-cdata
    for(m in mas){
        ldata<-merge(ldata,SMA(cdata,m))
    }
    names(ldata)<-c('Value',paste('ma',mas,sep=''))
    return(ldata)
}

title<-'000001.SZ'
SZ000001<-data[[title]]                            
sDate<-as.Date("2015-01-01")      
eDate<-as.Date("2015-07-10")                     
cdata<-SZ000001[dateArea(sDate,eDate,360)]$Close   
ldata<-ma(cdata,c(5,20,60))                 
tail(ldata)

drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=max(index(ldata)),breaks="1 year",avg=FALSE,out=FALSE){
    if(sDate<min(index(ldata))) sDate=min(index(ldata))
    if(eDate>max(index(ldata))) eDate=max(index(ldata))  
    ldata<-na.omit(ldata)
    
    g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
    g<-g+geom_line()
    g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))

    if(avg){
        meanVal<<-round(mean(ldata[dateArea(sDate,eDate)]$Value),2)
        g<-g+geom_hline(aes(yintercept=meanVal),color="red",alpha=0.8,size=1,linetype="dashed")
        g<-g+geom_text(aes(x=sDate, y=meanVal,label=meanVal),color="red",vjust=-0.4)
    }
    g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks(breaks),limits = c(sDate,eDate))
    g<-g+ylim(min(ldata$Value), max(ldata$Value))
    g<-g+xlab("") + ylab("Price")+ggtitle(title)
    g
}

drawLine(ldata,title,sDate,eDate,'1 month',TRUE) 

getMaSd<-function(ldata,mas=20,sDate,eDate){
  if(is.null(ldata) || nrow(ldata)<= max(mas)) return(NULL)          
  col<-paste('ma',mas,sep='')
  ldata<-ldata[,c("Value",col)]                     
  ldata$dif<-ldata[,col]-ldata$Value          
  ldata$sd<-runSD(ldata[,"dif"],mas)               
  ldata$rate<-round(ldata$dif/ldata$sd,2)        
  ldata[dateArea(sDate,eDate)]                   
}


ldata5<-getMaSd(ldata,5,sDate,eDate)
head(ldata5)

ldata20<-getMaSd(ldata,20,sDate,eDate)
head(ldata20)

ldata60<-getMaSd(ldata,60,sDate,eDate)
head(ldata60)

buyPoint<-function(ldata,x=2,dir=2){     
    idx<-which(ldata$rate>x)           
    if(dir==2){                      
        idx<-c(idx,which(ldata$rate<x*-1))
    }
    return(ldata[idx,])                                  
}

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

buydata<-buyPoint(ldata20,2,2)      
drawPoint(ldata20[,c(1,2)],buydata$Value,title,sDate,eDate,'1 month')

buydata<-buyPoint(ldata20,2,1)
drawPoint(ldata20[,c(1,2)],buydata$Value,title,sDate,eDate,'1 month')

sellPoint<-function(ldata,buydata){  
    buy<-buydata[which(buydata$dif>0),]

    aidx<-index(ldata[which(ldata$dif<=0),])
    sellIdx<-sapply(index(buy),function(ele){
        head(which(aidx>ele),1)
    })
    ldata[aidx[unique(unlist(sellIdx))]]
}


selldata<-sellPoint(ldata20,buydata)
selldata

bsdata<-merge(buydata$Value,selldata$Value)
names(bsdata)<-c("buy","sell")
drawPoint(ldata20[,c(1,2)],bsdata,title,sDate,eDate,'1 month')

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

    return(list(
        ticks=ticks,
        rise=rise,
        fall=fall
    ))
}

result<-trade(sdata,100000,10000)  
result$ticks
result$rise
result$fall

drawAsset<-function(ldata,adata,sDate=FALSE,capital=100000){
  if(!sDate) sDate<-index(ldata)[1]
  adata<-rbind(adata,as.xts(capital,as.Date(sDate)))
  
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(x=as.Date(Index), y=Value,colour=Series),data=fortify(adata,melt=TRUE))
  g<-g+facet_grid(Series ~ .,scales = "free_y")
  g<-g+scale_y_continuous(labels=dollar_format(prefix = "￥"))
  g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
  g<-g+xlab("") + ylab("Price")+ggtitle(title)
  g
}

drawAsset(ldata20,as.xts(result$ticks['asset'])) 

quick<-function(title,sDate,eDate){ 
  stock<-data[[title]]
  cdata<-stock[dateArea(sDate,eDate,360)]$Close
  ldata<-ma(cdata,c(20))
  ldata<-getMaSd(ldata,20,sDate,eDate)
  buydata<-buyPoint(ldata,2,1)  
  selldata<-sellPoint(ldata,buydata)
  sdata<-signal(buydata,selldata)
  return(trade(sdata))
}


title<-"300104.SZ"
sDate<-as.Date("2015-01-01")
eDate<-as.Date("2015-07-10")
quick(title,sDate,eDate)

stock<-data[[title]]
cdata<-stock[dateArea(sDate,eDate,360)]$Close
ldata<-ma(cdata,c(20))
ldata<-getMaSd(ldata,20,sDate,eDate)
buydata<-buyPoint(ldata,2,1)  
selldata<-sellPoint(ldata,buydata)
bsdata<-merge(buydata$Value,selldata$Value)
names(bsdata)<-c("buy","sell")
drawPoint(ldata[,c(1,2)],bsdata,title,sDate,eDate,'1 month')


sDate<-as.Date("2015-01-01")   
eDate<-as.Date("2015-07-10") 

data0<-lapply(data,function(stock){         
    cdata<-stock[dateArea(sDate,eDate,360)]$Close
    ldata<-ma(cdata,c(5,20))
    getMaSd(na.omit(ldata),20,sDate,eDate)
})
data0<-data0[!sapply(data0, is.null)]      

length(data)
length(data0)
head(data0[[1]])

buys<-lapply(data0,function(stock){ 
    if(nrow(stock)==0) return(NULL)
    buy<-buyPoint(stock,2,1)
    if(nrow(buy)>0) {
      return(buy)
    }
})

buys<-buys[!sapply(buys, is.null)] 
length(buys)
head(buys)

buydf<-ldply(buys,function(e){  
  data.frame(date=index(e), coredata(e))
}) 

buydatas<-ddply(buydf, .(date), function(row) {  
  row[row$rate == max(row$rate ),]
}) 

nrow(buydatas)
head(buydatas)

selldatas<-data.frame()
for(i in 1:nrow(buydatas)){
  buydata<-buydatas[i,]
  if(is.data.frame(buydata)){
      buydata<-xts(buydata,order.by=as.Date(buydata$date))
  }
  
  ldata<-data0[[buydata$.id]] 
  sell<-sellPoint(ldata,buydata)
  
  if(nrow(sell)>0){
      sell<-data.frame(sell,.id=buydata$.id,date=index(sell))
      selldatas<-rbind(selldatas,sell)
    }
}

selldatas<-unique(selldatas)  
nrow(selldatas)
head(selldatas)

buydatas$op<-'B'                
selldatas$op<-'S'               
sdatas<-rbind(buydatas,selldatas) 
row.names(sdatas)<-1:nrow(sdatas) 
sdatas<-sdatas[order(sdatas$.id),] 
head(sdatas)
slist<-split(sdatas[-1],sdatas$.id)
results<-lapply(slist,trade)
names(results)
results[['000002.SZ']]$ticks
quick('002234.SZ',sDate,eDate)$ticks




