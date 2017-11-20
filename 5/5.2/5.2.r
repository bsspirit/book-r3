library(ggplot2)
library(scales)

sh<-read.table(file="sh_bond.csv",header=FALSE,sep=",",colClasses = "character",fileEncoding="utf-8", encoding = "utf-8")
names(sh)<-c("名称","代码","简称","银行间代码","银行间简称","到期日期")
sh$到期日期<-as.Date(sh$到期日期,format="%Y-%m-%d")

sz<-read.table(file="sz_bond.csv",header=FALSE,sep=",",colClasses = "character",fileEncoding="utf-8", encoding = "utf-8")
names(sz)<-c("名称","代码","简称","银行间代码","银行间简称","到期日期")
sz$到期日期<-as.Date(sz$到期日期,format="%Y-%m-%d")

ss<-rbind(sh,sz)
head(ss)

tmp<-ss[order(ss$到期日期),]      
head(tmp[,c(2,3,6)],10)

g<-ggplot(ss, aes(x=到期日期))
g<-g+geom_histogram(binwidth=50,position="identity")
g<-g+scale_x_date(breaks = date_breaks(width="1 year"),labels = date_format("%Y"), limits = c(as.Date("2014-01-01"),as.Date("2024-01-01")))
g<-g+xlab("到期日期")+ylab("债券数量")
g

ss$付息日期<-as.Date(paste(2014,format(ss$到期日期,format='-%m-%d'),sep=""))
tmp<-ss[order(ss$付息日期),]     
head(tmp[,c(2,3,6,7)],20)

g<-ggplot(ss, aes(x=付息日期))
g<-g+geom_histogram(binwidth=1,position="identity")
g<-g+scale_x_date(breaks=date_breaks("1 month"),labels = date_format("%Y%m"), limits = c(as.Date("2014-01-01"),as.Date("2014-12-31")))
g<-g+xlab("付息日期")+ylab("债券数量")
g

