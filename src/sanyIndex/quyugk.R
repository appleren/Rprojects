library(plyr)
library(reshape2)
library(ggplot2)
source("functions.R")

#=========3 gdp==============
xiangmushu<-read.csv("data/xiangmushu.csv", header=TRUE)
zhufangmianji<-read.csv("data/zhufangmianji.csv", header=TRUE)
zhufangtouzi<-read.csv("data/zhufangtuozi.csv", header=TRUE)

xiangmushu<-subset(xiangmushu, select=c("Province", "Level", "year", "month", "Month"))
colnames(xiangmushu)<-c("province", "xiangmushu", "year", "month", "Month")
zhufangmianji<-subset(zhufangmianji, select=c("Province", "Level", "year", "month", "Month"))
colnames(zhufangmianji)<-c("province", "zhufangmianji", "year", "month", "Month")
zhufangtouzi<-subset(zhufangtouzi, select=c("Province", "Level", "year", "month", "Month"))
colnames(zhufangtouzi)<-c("province", "zhufangtouzi", "year", "month", "Month")

econ<-merge(xiangmushu, zhufangmianji, by=c("province", "Month", "year", "month"), all.x=TRUE)
econ<-merge(econ, zhufangtouzi, by=c("province", "Month", "year", "month"), all.x=TRUE)
econ[is.na(econ)]<-0
econ<-subset(econ, !(year==2015&month==7))
write.csv(econ, "data/mid/econ.csv", row.names=FALSE)

y<-ddply(econ, .(province), function(x){
  for (i in c(5:7)){
    x[,i]<-c(0,diff(x[,i],1))
  }
  x
})
y<-subset(y,month>2)
y<-subset(y, !(year==2009&month==10))
# y1<-subset(y, province=="Beijing")


#===========1 operate rate===================
bc_or<-read.csv("data/bc_operator_rate.csv", header=TRUE)
colnames(bc_or)[1]<-"rate"
d<-colnames(bc_or)[-1]
d<-substr(d, 2, 5)
t1<-t(data.frame(bc_or, row.names=1))
t2<-as.data.frame(t1, row.names=F)
#rete data and provicne
t3<-cbind(d, t2)

#=====================
bc<-melt(t3, id.vars=c("d"))
bc$year<-as.numeric(paste("20",substr(bc$d, 1,2), sep=""))
bc$month<-as.numeric(substr(bc$d, 3, 4))
colnames(bc)<-c("date", "province", "rate", "year", "month")
write.csv(bc, "data/mid/bcrate.csv", row.names=FALSE)
write.csv(t3, "data/mid/t3.csv", row.names=FALSE)

#=============province rate ccf========================
t3[is.na(t3)]<-0
provRatet<-removeSeasonalDF(standardizationDF(t3, id.vars=c("d")), 10, id.vars=c("d"), rt=c("t"))
cor(provRatet[, -1])
calmcor(provRatet[, -1], provRatet[, -1], "data/result/provRateT.csv")

#============merge bc and gdp==============
mapping<-read.csv("data/provc.csv")
bc<-subset(bc, month>2)
y$province<-as.character(y$province)
for(i in 1:length(mapping[,1])){
  m<-mapping[i,]
  y$province[which(y$province==m$c1)]<-as.character(m$c2)
}
write.csv(y, "data/mid/y.csv", row.names=FALSE)
yb<-merge(bc, y, by=c("province", "year", "month"))
yb<-yb[order(yb[,1],yb[,2], yb[,3]),]
yb<-subset(yb, select=c(-4,-6 ))

# x<-subset(yb, province=="北京")
rcor<-ddply(yb, .(province), function(x) {
  prov<-x$province[1]
  x[is.na(x)]<-0
  x$rateCumSum<-cumsum(x$rate)
  x<-removeSeasonalDF(standardizationDF(x, id.vars=c("province", "year", "month")), 10, id.vars=c("province", "year", "month"), rt=c("t"))
  calmcor(x[,c("xiangmushu", "zhufangmianji", "zhufangtouzi")], x[, c("rate", "rateCumSum")], paste("data/result/", prov, "cor.csv", sep=""), FALSE)
#   calCCF(x[,c("xiangmushu", "zhufangmianji", "zhufangtouzi")], x[, c("rate", "rateCumSum")], paste("data/result/", prov, "ccf.csv", sep=""))
})
write.csv(rcor, "data/result/rcor.csv")

rccf<-ddply(yb, .(province), function(x) {
  prov<-x$province[1]
  x[is.na(x)]<-0
  x$rateCumSum<-cumsum(x$rate)
  x<-removeSeasonalDF(standardizationDF(x, id.vars=c("province", "year", "month")), 10, id.vars=c("province", "year", "month"), rt=c("t"))
#   calmcor(x[,c("xiangmushu", "zhufangmianji", "zhufangtouzi")], x[, c("rate", "rateCumSum")], paste("data/result/", prov, "cor.csv", sep=""))
  calCCF(x[,c("xiangmushu", "zhufangmianji", "zhufangtouzi")], x[, c("rate", "rateCumSum")], paste("data/result/", prov, "ccf.csv", sep=""), FALSE)
})
write.csv(rccf, "data/result/rccf.csv")