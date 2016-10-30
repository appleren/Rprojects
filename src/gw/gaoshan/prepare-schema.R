library(plyr)
library(stringdist)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)



datapath<-"../../data/gw/gaoshan"
resultpath<-"../../data/gw/gaoshan/result"

#============================预处理原始数据==============================
folders<-list.dirs(path=paste0(datapath, "/schm"))
finalresult<-data.frame()
for(i in 1:length(folders)) {
  filenames<-list.files(path=folders[i], pattern=".txt", full.names=TRUE)
  if(length(filenames)>0) {
    result<-data.frame()
    for(j in 1:length(filenames)) {
      data<-read.table(filenames[j], sep=";")
      colnames(data)<-c("id", "date", "tid", "datacount", "colcount", "colnames")
      data$c<-c(1,diff(data$id))
      data$c[which(data$c!=0)]<-1
      data$c<-cumsum(data$c)
      r<-ddply(data, .(tid,c), function(d) {
        data.frame(tid=d$tid[1], id=d$id[1], from=min(d$date), to=max(d$date), datacount=sum(d$datacount), colcount=d$colcount[1], colnames=d$colnames[1])
      })
      result<-rbind(result, r)
    }
  }
  if(exists("result")) {
    finalresult<-rbind(finalresult, result)
  }
}

finalresult<-finalresult[,-1]
write.csv(finalresult, paste0(resultpath, "/result.csv"), row.names = FALSE)

sorted_result<-ddply(finalresult, .(tid), function(d) {
  if(length(d$tid)==1) {
    return (d)
  } else {
    colname<-colnames(d)
    d<-d[order(d$from),]
    d$diff<-c(1, diff(d$id))
    d$diff[which(d$diff!=0)]<-1
    d$diff<-cumsum(d$diff)
    r<-ddply(d, .(diff), function(m) {
      data.frame(tid=m$tid[1], from=min(m$from), to=max(m$to), datacount=sum(m$datacount), colcount=m$colcount, id=m$id[1], colnames=m$colnames[1])
    })
    return (r[,-1])
  }
})

write.csv(sorted_result, paste0(resultpath, "/sortedResult.csv"), row.names=FALSE)

#========================处理风机分类数据==================
propaths_2<-read.csv(paste0(datapath, "/propaths_wtinfo/propaths_2.csv"))
propaths_3<-read.csv(paste0(datapath, "/propaths_wtinfo/propaths_3.csv"))

sub_2<-subset(propaths_2, select = c("protocolid", "datapath", "descrcn", "compath", "ascflg"))
colnames(sub_2)<-c("protocolid", "iecpath", "descrcn", "compath", "ascflg")
sub_2$iecpath<-as.character(sub_2$iecpath)
sub_2$descrcn<-as.character(sub_2$descrcn)
sub_3<-subset(propaths_3, select = c("protocolid", "iecpath", "descrcn", "compath", "ascflg"))
sub_3$iecpath<-as.character(sub_3$iecpath)
sub_3$descrcn<-as.character(sub_3$descrcn)
propaths<-rbind(sub_2, sub_3)

#=====================按照协议演化关系处理协议变量名=================
pro_type<-read.csv(paste0(datapath, "/propaths_wtinfo/protocol_type.csv"))
# length(levels(as.factor(pro_type$protocolid))) #121
# length(levels(as.factor(propaths$protocolid))) #131
length(levels(as.factor(wtinf$protocolid)))
pro_type$group<-as.character(pro_type$group)
propaths_merged<-merge(propaths, pro_type, by="protocolid", all.x=TRUE)
wtinf<-read.csv(paste0(datapath, "/propaths_wtinfo/wtinf.csv"))
wtinf_merged<-merge(wtinf, pro_type, by="protocolid", all.x=TRUE)

write.csv(wtinf_merged, paste0(resultpath, "wtinf_withgroup.csv"), row.names = FALSE)
write.csv(propaths_merged, paste0(resultpath, "propaths_withgroup.csv"), row.names = FALSE)

#=====================按照分组和协议处理实际schema====================
tmp<-read.csv(paste0(resultpath, "/sortedResult.csv"))
tmp$fid<-as.integer(substr(as.character(tmp$tid), 1, 6))
wtinf_merged<-read.csv(paste0(resultpath, "/wtinf_withgroup.csv"))
propaths_merged<-read.csv(paste0(resultpath, "/propaths_withgroup.csv"))

# length(levels(as.factor(tmp$tid))) #6179
# length(levels(as.factor(wtinf_merged$wtid))) #12613
colnames(wtinf_merged)[2:3]<-c("fid", "tid")
tmp_type<-merge(tmp, wtinf_merged, by=c("tid", "fid"), all.x = TRUE)
tid_no_protocol<-subset(tmp_type, is.na(protocolid))
write.csv(tid_no_protocol, paste0(resultpath, "tid_no_protocol.csv"), row.names = FALSE)

#分开列名和data type
col<-as.character(tmp_type$colnames)[1]
splitCol<-function(col, splitor1=",", splitor2=" ") {
  name_types<-unlist(str_split(col, splitor1))
  test<-unlist(llply(name_types, str_split, " "))
  # test<-llply(name_types, str_split, " ")
  if(length(test)%%2==0) {
    t<-length(test)-1
  } else {
    t<-length(test)
  }
  names<-test[seq(from=1, to=t, by=2)]
  types<-test[seq(from=2, to=(length(test)%/%2)*2, by=2)]
}
