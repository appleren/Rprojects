library(XLConnect)

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

write.csv(stationRunningParam, paste(datapath, "cz1_mid/场站运行参数.csv", sep=""))
write.csv(clientOutput, paste(datapath, "cz1_mid/客户分输.csv", sep=""))
write.csv(compressorParam, paste(datapath, "cz1_mid/压缩机运行参数.csv", sep=""))
write.csv(consumption, paste(datapath, "cz1_mid/能耗.csv", sep=""))
write.csv(gasCondition, paste(datapath, "cz1_mid/气质分析报告.csv", sep=""))


