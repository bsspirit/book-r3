library(rjson)
json <- fromJSON(paste(readLines("cb.json"), collapse=""))
df <- data.frame(matrix(unlist(json), nrow=length(json), byrow=TRUE), stringsAsFactors = FALSE)
names(df)<-names(json[[1]])
str(df)

df$volume<-as.numeric(df$volume)
df$current<-as.numeric(df$current)
df$change<-as.numeric(df$change)
df$percent<-as.numeric(df$percent)
df$high<-as.numeric(df$high)
df$low<-as.numeric(df$low)
df$high52w<-as.numeric(df$high52w)
df$low52w<-as.numeric(df$low52w)

df$open<-as.numeric(df$open)
df$kzz_stock_current<-as.numeric(df$kzz_stock_current)
df$kzz_convert_price<-as.numeric(df$kzz_convert_price)
df$kzz_covert_value<-as.numeric(df$kzz_covert_value)
df$kzz_cpr<-as.numeric(df$kzz_cpr)
df$kzz_putback_price<-as.numeric(df$kzz_putback_price)
df$kzz_redempt_price<-as.numeric(df$kzz_redempt_price)

df$kzz_straight_price<-as.numeric(df$kzz_straight_price)
df$kzz_stock_percent<-as.numeric(df$kzz_stock_percent)
df$pb<-as.numeric(df$pb)
df$net_assets<-as.numeric(df$net_assets)
df$benefit_before_tax<-as.numeric(df$benefit_before_tax)

df$benefit_after_tax<-as.numeric(df$benefit_after_tax)
df$convert_bond_ratio<-as.numeric(df$convert_bond_ratio)
df$totalissuescale<-as.numeric(df$totalissuescale)
df$outstandingamt<-as.numeric(df$outstandingamt)
df$remain_year<-as.numeric(df$remain_year)
df$convertrate<-as.numeric(df$convertrate)

library(stringr)
library(magrittr)

df2<-df[,c('kzz_convert_time')]

df2 %<>% str_split(pattern='-') %>% 
  unlist %>% matrix(ncol=2, byrow=TRUE) %>% 
  data.frame(stringsAsFactors=FALSE) %>% 
  cbind(df$symbol,df$name)

today<-Sys.Date();today
df2 %<>% {
  df2$X1<-as.Date(df2$X1,format='%Y.%m.%d')
  df2$X2<-as.Date(df2$X2,format='%Y.%m.%d')
  df2[intersect(which(df2$X1 < today),which(df2$X2 > today)),]
}


df3<-df[which(df$symbol %in% df2$`df$symbol`),]
df3$name2<-str_c(df3$name,'(',df3$code,')')
df3$time<-format(Sys.time(),'%H:%M:%S')
df3$current2<-str_c(df3$current,'(',df3$percent,')')
df3$current3<-str_c(df3$kzz_stock_current,'(',df3$kzz_stock_percent,')')
  
cols<-c('name2','time','current2','current3','kzz_cpr')
df4<-df3[order(df3$kzz_cpr),cols]
names(df4)<-c('转债名称','更新时间','转债价格(涨幅)','正股价格(涨幅)','溢价率')
df4

library(PerformanceAnalytics)
df110030<-read.csv(file="110030.csv",header=TRUE)
x110030<-xts(df110030[,c('收盘价','转股价值')],order.by = as.Date(as.character(df110030$����),format='%Y-%m-%d'))
x110030<-na.omit(x110030)
names(x110030)<-c('close','value')

df600185<-read.csv(file="600185-2.csv",header=TRUE)
x600185<-xts(df600185$close,order.by = as.Date(as.character(df600185$date),format='%Y-%m-%d'))
names(x600185)<-'stock'
x110030<-merge(x110030,x600185,all = FALSE)
head(x110030)

x110030$convetPrice<-NA
x110030$convetPrice['2014-12-25']<-20.9
x110030$convetPrice['2016-05-26']<-7.39
x110030$convetPrice['2016-08-25']<-7.26
x110030<-na.locf(x110030)


x110030$Premium<-(x110030$close - x110030$value)/x110030$value
x110030$expRet<-(x110030$value-x110030$close)/x110030$close
x110030$ret<-Return.calculate(x110030$close,method="log")
x110030$ret[which(is.na(x110030$ret))]<-0
x110030$ret<-cumprod(1 + x110030$ret)-1

x110030$vol <-volatility(x110030$close,n=20)
x110030$loss<- pnorm(-1 * x110030$expRet, 0, x110030$vol)
x110030$sharpe<-x110030$expRet/x110030$vol*sqrt(250)

x110030$Premium<-x110030$Premium*100
x110030$expRet<-x110030$expRet*100
x110030$ret<-x110030$ret*100
x110030$vol<-x110030$vol*100
x110030$loss<-x110030$loss*100

tail(x110030)
x110030[which(x110030$expRet>0),]

library(ggplot2)
library(scales)

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(x110030[,c('close','value','Premium')],melt=TRUE))
g<-g+geom_line(size=1)
g 

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(x110030[,c('value','stock','convetPrice')],melt=TRUE))
g<-g+geom_line(size=1)+scale_y_log10()
g 

g<-ggplot(aes(x=Index,y=Value, colour=Series),data=fortify(x110030[,c('ret','expRet','vol','loss','sharpe')],melt=TRUE))
g<-g+geom_line(size=1)
g 
