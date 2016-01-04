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
source("accuracy.R")
#-------------------------nnet-----------------------------------
# ���ܺķ���  
# 
# ѹ��  ����վ��ѹ��
# ѹ��  ����վ��ѹ��
# ������	ת�ٳ���ʱ�����ѹ��
# ��վ�¶�	��վ�¶�
# ��վ�¶�	��վ�¶�
# ����      ����
# ������	�ܷ�����
# ��������	�������ʱ������BWRS
# 
# ��ѹ�����ܺ�	���ѹ�����ܺĺ�
# �ܺ���	�ܺ���

fd<-read.csv(paste(datapath, "cz1_mid/fd.csv", sep=""), header=TRUE)

fd<-subset(fd, select=c("date", "allCons", "all_p", "p_percent", 
                        "p_diff", "workload", "cz_earthtemp",
                        "fs_qty", "year", "month", "day"))

wholeData<-data.frame(scale(subset(fd, select=c(-1, -3, -9, -10, -11))))#allCons

calnnet(wholeData, allCons~., fd$allCons, "���ܺ�Ԥ��")

#�ܺ���
fd<-read.csv(paste(datapath, "cz1_mid/fd.csv", sep=""), header=TRUE)

fd<-subset(fd, select=c("date", "allCons", "all_p", "p_percent", 
                        "p_diff", "workload", "cz_earthtemp",
                        "fs_qty", "year", "month", "day", "CH4", "water_dew_point"))
#TODO �ֶ���
fd2<-subset(fd, !month%in%c(11,12,1,2,3), select=c(-1, -2, -4, -5, -9, -10, -11))
wholeData<-data.frame(scale(fd2))#all_p
calnnet(wholeData, all_p~., fd2$all_p, "�ܺ���Ԥ��")

fd2<-subset(fd, month%in%c(11,12,1,2,3), select=c(-1, -2, -4, -5, -9, -10, -11))
wholeData<-data.frame(scale(fd2))#all_p
calnnet(wholeData, all_p~., fd2$all_p, "�ܺ���Ԥ��")

# fd2<-subset(fd, select=c(-1, -2, -5, -9, -10, -11))
# wholeData<-data.frame(scale(fd2))#all_p
# calnnet(wholeData, all_p~., fd2$all_p, "�ܺ���Ԥ��")


# ��λʱ���ܺķ���  
# 
# ��������  �������ʱ������BWRS
# ������  ѹ�ȳ���ת��
# ѹ��  ��������ѹ��
# ѹ��  ��������ѹ��
# 
# ��λʱ���ܺ�	�����ܺĳ���ʱ��

fd<-read.csv(paste(datapath, "cz1_mid/fd1.csv", sep=""), header=TRUE)
fd1<-subset(fd, nh1<2, select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
#-------------------------nnet-----------------------------------

# ��λʱ���ܺķ���
# fd1<-fd1[1:397,]
wholeData<-data.frame(scale(subset(fd1, select=-1)))
calnnet(wholeData, nh1~., fd1$nh1, "��λʱ�䵥���ܺ�Ԥ��")

fd1<-subset(fd, nh1<2&comp_no=="1#", select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
wholeData<-data.frame(scale(subset(fd1, select=-1)))
calnnet(wholeData, nh1~., fd1$nh1, "1#��λʱ�䵥���ܺ�Ԥ��")

fd1<-subset(fd, nh1<2&comp_no=="2#", select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
wholeData<-data.frame(scale(subset(fd1, select=-1)))
calnnet(wholeData, nh1~., fd1$nh1, "2#��λʱ�䵥���ܺ�Ԥ��")

fd1<-subset(fd, nh1<2&comp_no=="3#", select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
wholeData<-data.frame(scale(subset(fd1, select=-1)))
calnnet(wholeData, nh1~., fd1$nh1, "3#��λʱ�䵥���ܺ�Ԥ��")

fd1<-subset(fd, nh1<2&comp_no=="4#", select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
wholeData<-data.frame(scale(subset(fd1, select=-1)))
calnnet(wholeData, nh1~., fd1$nh1, "4#��λʱ�䵥���ܺ�Ԥ��")

fd1<-subset(fd, nh1<2&comp_no=="5#", select=c("date", "nh1", "workload", "p_percent", "p_diff", "comp_intemp", "comp_inP", "comp_speed", "CH4", "water_dew_point"))
wholeData<-data.frame(scale(subset(fd1, select=-1)))
calnnet(wholeData, nh1~., fd1$nh1, "5#��λʱ�䵥���ܺ�Ԥ��")



# colnames(wholeData)
# plot(wholeData$all_p, type="l")
# lines(wholeData$p_percent, col="red")
# lines(wholeData$p_diff, col="green")
# lines(wholeData$workload, col="blue")
# lines(wholeData$cz_intemp, col="blue")
# lines(wholeData$cz_outtemp, col="blue")
# lines(wholeData$cz_earthtemp, col="blue")
# lines(wholeData$fs_qty, col="blue")