library(XLConnect)
library(reshape2)
library(zoo)
library(plyr)
library(nnet)
library(rpart)

datapath<-"../../data/cnpc/nenghao/"
filenames<-list.files(path=paste(datapath, "cz1/", sep=""), pattern=".xls")
stationRunningParam<-data.frame()
clientOutput<-data.frame()
compressorParam<-data.frame()
consumption<-data.frame()
gasCondition<-data.frame()

for(filename in filenames) {
  d<-as.Date(ISOdate(substr(filename, 1, 4), substr(filename, 5,6), substr(filename, 7,8)))
  #read xls file
  xls <- loadWorkbook(paste(datapath, "cz1/", filename, sep=""),create=FALSE)
  
  #场站运行参数
  s<-readWorksheet(xls, "Sheet1", region="c4:d8", header=FALSE)
  colnames(s)<-c("para_name", "value")
  s<-dcast(s, .~para_name)
  s<-subset(s, select=-1)
  colnames(s)<-paste("场站", colnames(s), sep="_")
  s$date<-rep(d, form=1, to=length(s$linename))
  stationRunningParam<-rbind(stationRunningParam, s)
  
  #客户分输
  s<-readWorksheet(xls, "Sheet1", region="c10:C10", header=FALSE)
  clientName<-s[1,1]
  s<-readWorksheet(xls, "Sheet1", region="c11:d13", header=FALSE)
  colnames(s)<-c("para_name", "value")
  s<-dcast(s, .~para_name)
  s<-subset(s, select=-1)
  colnames(s)<-paste("客户分输", colnames(s), sep="_")
  s$client_name<-clientName
  s$date<-rep(d, form=1, to=length(s$linename))
  clientOutput<-rbind(clientOutput, s)
  
  s<-readWorksheet(xls, "Sheet1", region="c14:C14", header=FALSE)
  clientName<-s[1,1]
  s<-readWorksheet(xls, "Sheet1", region="c15:d17", header=FALSE)
  colnames(s)<-c("para_name", "value")
  s<-dcast(s, .~para_name)
  s<-subset(s, select=-1)
  colnames(s)<-paste("客户分输", colnames(s), sep="_")
  s$client_name<-clientName
  s$date<-rep(d, form=1, to=length(s$linename))
  clientOutput<-rbind(clientOutput, s)
  
  s<-readWorksheet(xls, "Sheet1", region="c18:C18", header=FALSE)
  clientName<-s[1,1]
  s<-readWorksheet(xls, "Sheet1", region="c19:d21", header=FALSE)
  colnames(s)<-c("para_name", "value")
  s<-dcast(s, .~para_name)
  s<-subset(s, select=-1)
  colnames(s)<-paste("客户分输", colnames(s), sep="_")
  s$client_name<-clientName
  s$date<-rep(d, form=1, to=length(s$linename))
  clientOutput<-rbind(clientOutput, s)
  
  #压缩机运行参数
  s<-readWorksheet(xls, "Sheet1", region="b23:d57", header=FALSE)
  colnames(s)<-c("comp_no", "para_name", "value")
  s$comp_no<-na.locf(s$comp_no)
  s<-subset(s, !is.na(para_name))
  s<-dcast(s, comp_no~para_name)
  colnames(s)<-paste("压缩机", colnames(s), sep="_")
  s$date<-rep(d, form=1, to=length(s$linename))
  compressorParam<-rbind(compressorParam, s)
  
  #能耗
  s<-readWorksheet(xls, "Sheet1", region="c59:d66", header=FALSE)
  colnames(s)<-c("para_name", "value")
  s<-dcast(s, .~para_name)
  s<-subset(s, select=-1)
  s$date<-rep(d, form=1, to=length(s$linename))
  consumption<-rbind(consumption, s)
  
  #气质报告
  s<-readWorksheet(xls, "Sheet1", region="c68:d82", header=FALSE)
  colnames(s)<-c("para_name", "value")
  s<-dcast(s, .~para_name)
  s<-subset(s, select=-1)
  s$date<-rep(d, form=1, to=length(s$linename))
  gasCondition<-rbind(gasCondition, s)
}

# stationRunningParam<-data.frame() 场站运行参数
# clientOutput<-data.frame() 客户分输
# compressorParam<-data.frame() 压缩机运行参数
# consumption<-data.frame() 能耗
# gasCondition<-data.frame() 气质分析报告

colnames(stationRunningParam)<-c("cz_outtemp", "cz_outP", "cz_earthtemp", "cz_intemp", "cz_inP", "date")
colnames(clientOutput)<-c("fs_qty", "fs_temp", "fs_p", "client_name", "date")
colnames(compressorParam)<-c("comp_no", "comp_outtemp", "comp_outP", "comp_intemp", "comp_inP", "comp_time", "comp_speed", "date")
colnames(consumption)<-c("other", "no1", "no2", "no3", "no4", "no5", "all", "all_p", "date")
colnames(gasCondition)<-c("C2H6", "C3H8", "C6+", "CH4", "CO2", "H2S", "i-C4H10", "i-C5H12", "n-C4H10", "n-C5H12", "N2", "low_heat", "high_heat", "water_dew_point", "hydrocarbon_dew_point", "date")


write.csv(stationRunningParam, paste(datapath, "cz1_mid/场站运行参数.csv", sep=""))
write.csv(clientOutput, paste(datapath, "cz1_mid/客户分输.csv", sep=""))
write.csv(compressorParam, paste(datapath, "cz1_mid/压缩机运行参数.csv", sep=""))
write.csv(consumption, paste(datapath, "cz1_mid/能耗.csv", sep=""))
write.csv(gasCondition, paste(datapath, "cz1_mid/气质分析报告.csv", sep=""))

#----------------------Read data-------------------
consumption<-read.csv(paste(datapath, "cz1_mid/能耗.csv", sep=""))
stationRunningParam<-read.csv(paste(datapath, "cz1_mid/场站运行参数.csv", sep=""))
clientOutput<-read.csv(paste(datapath, "cz1_mid/客户分输.csv", sep=""))
compressorParam<-read.csv(paste(datapath, "cz1_mid/压缩机运行参数.csv", sep=""))
gasCondition<-read.csv(paste(datapath, "cz1_mid/气质分析报告.csv", sep=""))


consumption$allCons<-with(consumption, no1+no2+no3+no4+no5)

clientOutput<-aggregate(.~date, data=subset(clientOutput, select=(c("date", "fs_temp", "fs_p"))), FUN=mean)
# d1<-aggregate(.~date, 
#               data=subset(compressorParam,
#                           select=c("date", "comp_outtemp", 
#                                    "comp_outP", "comp_intemp", 
#                                    "comp_inP", "comp_speed")),
#               FUN=mean)
# d2<-aggregate(.~date,data=subset(compressorParam, select=c("date", "comp_time")), FUN=sum)
# compressorParam<-merge(d1, d2, by="date", all=TRUE)

compressorParam<-ddply(compressorParam, .(comp_no), function(d) {
  c<-paste("no", substr(d[1,1], 1, 1), sep="")
  m<-merge(d, subset(consumption, select=c("date", c)), by="date", all=TRUE)
  colnames(m)[10]<-"nh"
  m
})
compressorParam<-subset(compressorParam, nh!=0 & comp_time!=0 & comp_speed!=0)
compressorParam$nh1<-compressorParam$nh/compressorParam$comp_time
compressorParam$p_percent<-compressorParam$comp_inP/compressorParam$comp_outP
compressorParam$spp<-compressorParam$p_percent*compressorParam$comp_speed
gasCondition<-subset(gasCondition, select=c("date", "CH4", "C2H6", "high_heat", "water_dew_point"))
# fd<-merge(stationRunningParam, subset(consumption, select=c("allCons", "date")), by="date", all=TRUE)
# fd<-merge(fd, clientOutput, by="date", all=TRUE)
# fd<-merge(compressorParam, fd, by="date", x.all=TRUE)
# fd<-merge(fd, gasCondition, by="date", all=TRUE)
fd<-merge(compressorParam, gasCondition, by="date", x.all=TRUE)

# fd<-subset(fd, select=c(-2,-3,-12))
fd<-subset(fd, !is.na(nh)&nh1<2)
write.csv(fd, paste(datapath, "cz1_mid/fd.csv", sep=""))

#-------------------------nnet-----------------------------------

fd<-read.csv(paste(datapath, "cz1_mid/fd.csv", sep=""))
fd1<-data.frame(scale(subset(fd, select=-which(colnames(fd) %in% c("date", "comp_time", "X", "allCons", "nh", "comp_no", "comp_outP", "comp_inP", "comp_outtemp")))))
# fd1<-data.frame(subset(fd, select=-which(colnames(fd) %in% c("date", "comp_time", "X", "allCons", "nh", "comp_no", "comp_speed", "comp_outP", "comp_inP", "comp_outtemp"))))

trainN<-(length(fd1[,1])*80)%/%100

#nnet
nn<-nnet(nh1~., fd1[1:trainN,], size=10, decay=0.001, maxit=10000, linout=F, trace=F, MaxNWts=4000, na.action=na.omit)
p<-predict(nn, fd1[trainN+1:length(fd1[,1]),])
p<-p*sd(fd$nh1) + mean(fd$nh1)  
p1<-fitted(nn)
p1<-p1*sd(fd$nh1) + mean(fd$nh1)  
p2<-c(p1, p)
plot(fd$nh1, type="l")
lines(p2, col="red")

#rpart
rp<-rpart(nh1~., fd1[1:trainN,], method="anova")
q<-predict(rp, newdata=fd1[trainN+1:length(fd1[,1]),])
q<-q*sd(fd$nh1) + mean(fd$nh1) 
q1<-fitted(rp)
q1<-q1*sd(fd$nh1) + mean(fd$nh1)  
q2<-c(q1, q)
plot(fd$nh1, type="l")
lines(q2, col="red")