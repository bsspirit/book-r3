calc_net<-function(money,revenue){
  sum(money,revenue)
}
calc_netUnit<-function(net,share){
  net/share
}
calc_share<-function(share_list){
  sum(unlist(share_list))
}
calc_share_per<-function(money,retSum){
  money/ifelse(retSum==0,1,retSum)
}
calc_ret <- function(net_last1,revenue){
  revenue/net_last1
}
calc_retSum <- function(netUnit_last1,ret){
  (1+ret)*netUnit_last1
}
calc_asset_per<-function(netUnit,share_per){
  share_per*netUnit
}

Shares<-0
share_list<-list()
netUnit<-1        
revenue<-ret<-retSum<-0 
money_A<-money_B<-50000
share_list[['A']]<-calc_share_per(money_A,retSum)
share_list[['B']]<-calc_share_per(money_B,retSum)
Shares<-calc_share(share_list)
net<-calc_net(c(money_A,money_B),revenue)
ret;retSum;netUnit;net

revenue<-500
ret<-calc_ret(net,revenue)
retSum<-calc_retSum(netUnit,ret)
Shares<-calc_share(share_list)
net<-calc_net(net,revenue)
netUnit<-calc_netUnit(net,Shares)
ret;retSum;netUnit;net

revenue<-1000
money_C<-50000
ret<-calc_ret(net,revenue)
retSum<-calc_retSum(netUnit,ret)
share_list[['C']]<-calc_share_per(money_C,retSum)
Shares<-calc_share(share_list)
net<-calc_net(c(net,money_C),revenue)
netUnit<-calc_netUnit(net,Shares)
ret;retSum;netUnit;net
lapply(share_list,function(x) x* netUnit)

revenue<- -5000
ret<-calc_ret(net,revenue)
retSum<-calc_retSum(netUnit,ret)
net<-calc_net(net,revenue)
netUnit<-calc_netUnit(net,Shares)
ret;retSum;netUnit;net
lapply(share_list,function(x) x* netUnit)

