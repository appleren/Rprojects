library(XLConnect)
library(reshape2)
library(zoo)

datapath<-"data/gx1/"
filenames<-list.files(path=datapath, pattern=".xls")
pVendorIn<-data.frame() #油田输出
pStorageIn<-data.frame() #储气库进气量，应计入当日消耗
pVendorOut<-data.frame() #客户消耗
pStorageOut<-data.frame() #储气库出气量，应计入当日入气
pSelfConsume<-data.frame() #自身消耗 
pPipeStore<-data.frame() #管存Vc[i]=pPipeStore[i], Vm[i]=pPipeStore[i+1]

for(filename in filenames) {
  d<-as.Date(ISOdate(substr(filename, 1, 4), substr(filename, 5,6), substr(filename, 7,8)))
  #read xls file
  xls <- loadWorkbook(paste(datapath, filename, sep=""),create=FALSE)
  
  #income, vendor
  s<-readWorksheet(xls, "进气", region="B1:E3", header=TRUE)
  colnames(s)<-c("linename", "station", "vendor", "vendor_income")
  s$linename<-na.locf(s$linename)
  s$date<-rep(d, form=1, to=length(s$linename))
  pVendorIn<-rbind(pVendorIn, s)
  
  #storage
  st<-readWorksheet(xls, "进气", region="B5:E6", header=TRUE)
  colnames(st)<-c("linename", "station", "storagename", "storage_save")
  st$linename<-na.locf(st$linename)
  st$date<-rep(d, from=1, to=length(st$linename))
  pStorageIn<-rbind(pStorageIn, st)
  
  #output, client
  s<-readWorksheet(xls, "出气", region="B1:E22", header=TRUE)
  colnames(s)<-c("linename", "station", "client", "client_use")
  s$linename<-na.locf(s$linename)
  s$station<-na.locf(s$station)
  s$date<-rep(d, form=1, to=length(s$linename))
  pVendorOut<-rbind(pVendorOut, s)
  
  #storage
  st<-readWorksheet(xls, "出气", region="B25:E26", header=TRUE)
  colnames(st)<-c("linename", "station", "storagename", "storage_output")
  st$linename<-na.locf(st$linename)
  st$date<-rep(d, from=1, to=length(st$linename))
  pStorageOut<-rbind(pStorageOut, st)
  
  #self consume
  sc<-readWorksheet(xls, "自耗", region="A1:D15", header=TRUE)
  colnames(sc)<-c("linename", "linename2", "station", "daily_consume")
  sc$linename<-na.locf(sc$linename)
  sc$linename2<-na.locf(sc$linename2)
  sc$date<-rep(d, from=1, to=length(sc$linename))
  pSelfConsume<-rbind(pSelfConsume, sc)
  
  #pipe store
  ps<-readWorksheet(xls, "管存", region="A1:B3", header=TRUE)
  colnames(ps)<-c("gx", "daily_pipestore")
  ps$gx<-sub(linename, paste(linename, "_", sep=""), ps$gx)
  ps<-cbind(colsplit(ps$gx, "_", c("linename", "subline")), ps)
  ps$date<-rep(d, from=1, to=length(ps$linename))
  pPipeStore<-rbind(pPipeStore, ps)
}

# pVendorIn<-data.frame() #油田输出
# pStorageIn<-data.frame() #储气库进气量，应计入当日消耗
# pVendorOut<-data.frame() #客户消耗
# pStorageOut<-data.frame() #储气库出气量，应计入当日入气
# pSelfConsume<-data.frame() #自身消耗 
# pPipeStore<-data.frame() #管存Vc[i]=pPipeStore[i], Vm[i]=pPipeStore[i+1]

pVendorIn$vendor_income<-as.numeric(pVendorIn$vendor_income)
pStorageIn$storage_save<-as.numeric(pStorageIn$storage_save)
pVendorOut$client_use<-as.numeric(pVendorOut$client_use)
pStorageOut$storage_output<-as.numeric(pStorageOut$storage_output)
pSelfConsume$daily_consume<-as.numeric(pSelfConsume$daily_consume)
pPipeStore$daily_pipestore<-as.numeric(pPipeStore$daily_pipestore)


write.csv(pVendorIn, "data/gx1_mid/油田输出.csv")
write.csv(pStorageIn, "data/gx1_mid/储气库进气量.csv")
write.csv(pVendorOut, "data/gx1_mid/客户消耗.csv")
write.csv(pStorageOut, "data/gx1_mid/储气库出气量.csv")
write.csv(pSelfConsume, "data/gx1_mid/自身消耗.csv")
write.csv(pPipeStore, "data/gx1_mid/管存.csv")

#---------
pVendorOut<-aggregate(client_use~date+linename+station, data=pVendorOut, FUN=sum)
pPipeStore<-aggregate(daily_pipestore~date+linename, data=pPipeStore, FUN=sum)

r<-merge(pVendorIn, pStorageIn, by=c("date", "linename", "station"), all=TRUE)
r<-merge(r, pVendorOut, by=c("date", "linename", "station"), all=TRUE)
r<-merge(r, pStorageOut, by=c("date", "linename", "station"), all=TRUE)
r<-merge(r, pSelfConsume, by=c("date", "linename", "station"), all=TRUE)
r[is.na(r)]<-0

write.csv(r, "data/r.csv")

rg<-aggregate(.~date+linename, data=subset(r, select=c("vendor_income","storage_output","storage_save","client_use","daily_consume","date","linename")), FUN=sum)
rg<-merge(rg, pPipeStore, by=c("date", "linename"), all=TRUE)
write.csv(rg, "data/rg.csv")

