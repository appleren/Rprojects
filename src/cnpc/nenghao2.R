library(nnet)
library(rpart)
library(reshape2)
library(zoo)
library(plyr)
library(rpart.plot)
library(forecast)
library(reshape2)

#-------------Use nnet and rpart-------
datapath<-"../../data/cnpc/nenghao/"

#--------------------functions--------------
scale2<-function(d) {
  maxv<-max(d)
  minv<-min(d)
  r<-(d-minv)/(maxv-minv)
  r
}
revScale2<-function(d, r){
  maxv<-max(d)
  minv<-min(d)
  dd<-r*(maxv-minv)+minv
  dd
}
calMAE<-function(prd_err, original, line) {
  if(line==0) {
    prd_err<-prd_err[which(original!=0)]
    original<-original[which(original!=0)]
  }
  format(mean(abs(prd_err)/pmax(line,original)),digits=2)
}
calAccur<-function(prd_err, original,line) {
  format(sum(abs(prd_err)<=pmax(line,original*0.2))/length(original), digits=2)
}

calnnet<-function(wholeData, func, original, title){
  n<-length(wholeData[,1])
  trainLength<-(length(wholeData[,1])*80)%/%100
  
  nn<-nnet(func, wholeData[1:trainLength,], size=10, decay=0.0001, maxit=10000, linout=T, trace=F, MaxNWts=4000, na.action=na.omit)
  p<-predict(nn, wholeData)
  sdv<-sd(original)
  meanv<-mean(original) 
  p<-p*sdv + meanv
  pame<-calMAE(original[(trainLength+1):n]-p[(trainLength+1):n,1], original[(trainLength+1):n], 0)
  acc<-calAccur(original[(trainLength+1):n]-p[(trainLength+1):n,1], original[(trainLength+1):n], 0)
  plot(original, type="l", ylim=c(min(original, p), max(fd$all_p, p)), main=paste(title, "MAPE=",pame, ", accuracy=",acc, sep=""))
  lines(p, col="red")
}

#-------------------------nnet-----------------------------------
# 总能耗分析  
# 
# 压比  进出站气压比
# 压差  出进站气压差
# 工作量	转速乘以时间乘以压比
# 进站温度	进站温度
# 出站温度	出站温度
# 地温      地温
# 分输量	总分输量
# 气质物性	根据气质报告计算BWRS
# 
# 总压缩机能耗	五个压缩机能耗和
# 总耗气	总耗气

fd<-read.csv(paste(datapath, "cz1_mid/fd.csv", sep=""), header=TRUE)

fd<-subset(fd, select=c("date", "allCons", "all_p", "p_percent", 
                        "p_diff", "workload", "cz_earthtemp",
                        "fs_qty", "year", "month", "day"))

wholeData<-data.frame(scale(subset(fd, select=c(-1, -3, -9, -10, -11))))#allCons

calnnet(wholeData, allCons~., fd$allCons, "总能耗预测")

#总耗气
fd<-read.csv(paste(datapath, "cz1_mid/fd.csv", sep=""), header=TRUE)

fd<-subset(fd, select=c("date", "allCons", "all_p", "p_percent", 
                        "p_diff", "workload", "cz_earthtemp",
                        "fs_qty", "year", "month", "day", "CH4", "water_dew_point"))
#TODO 分段做
fd2<-subset(fd, !month%in%c(11,12,1,2,3), select=c(-1, -2, -4, -5, -9, -10, -11))
wholeData<-data.frame(scale(fd2))#all_p
calnnet(wholeData, all_p~., fd2$all_p, "总耗气预测")

fd2<-subset(fd, month%in%c(11,12,1,2,3), select=c(-1, -2, -4, -5, -9, -10, -11))
wholeData<-data.frame(scale(fd2))#all_p
calnnet(wholeData, all_p~., fd2$all_p, "总耗气预测")

# fd2<-subset(fd, select=c(-1, -2, -5, -9, -10, -11))
# wholeData<-data.frame(scale(fd2))#all_p
# calnnet(wholeData, all_p~., fd2$all_p, "总耗气预测")


# 单位时间能耗分析  
# 
# 气质物性  根据气质报告计算BWRS
# 工作量  压比乘以转速
# 压比  进出口气压比
# 压差  出进口气压差
# 
# 单位时间能耗	单机能耗除以时间

fd1<-read.csv(paste(datapath, "cz1_mid/fd1.csv", sep=""), header=TRUE)
fd1<-subset(fd1, nh1<2, select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
#-------------------------nnet-----------------------------------

# 单位时间能耗分析
# fd1<-fd1[1:397,]
wholeData<-data.frame(scale(subset(fd1, select=-1)))

calnnet(wholeData, nh1~., fd1$nh1, "单位时间单机能耗预测")


# colnames(wholeData)
# plot(wholeData$all_p, type="l")
# lines(wholeData$p_percent, col="red")
# lines(wholeData$p_diff, col="green")
# lines(wholeData$workload, col="blue")
# lines(wholeData$cz_intemp, col="blue")
# lines(wholeData$cz_outtemp, col="blue")
# lines(wholeData$cz_earthtemp, col="blue")
# lines(wholeData$fs_qty, col="blue")
