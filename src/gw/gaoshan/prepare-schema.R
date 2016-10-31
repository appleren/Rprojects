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

write.csv(wtinf_merged, paste0(resultpath, "/wtinf_withgroup.csv"), row.names = FALSE)
write.csv(propaths_merged, paste0(resultpath, "/propaths_withgroup.csv"), row.names = FALSE)

#=====================按照分组和协议处理实际schema====================
tmp<-read.csv(paste0(resultpath, "/sortedResult.csv"))
tmp$fid<-as.integer(substr(as.character(tmp$tid), 1, 6))
wtinf_merged<-read.csv(paste0(resultpath, "/wtinf_withgroup.csv"))
propaths_merged<-read.csv(paste0(resultpath, "/propaths_withgroup.csv"))

# length(levels(as.factor(tmp$tid))) #6179
# length(levels(as.factor(wtinf_merged$wtid))) #12613
colnames(wtinf_merged)[2:3]<-c("fid", "tid")
tmp_type<-merge(tmp, wtinf_merged, by=c("tid", "fid"), all.x = TRUE)

#异常数据，缺少协议
tid_no_protocol<-subset(tmp_type, is.na(protocolid))
write.csv(tid_no_protocol, paste0(resultpath, "/tid_no_protocol.csv"), row.names = FALSE)

#暂时只处理有协议的数据
tmp_type_good<-subset(tmp_type, !is.na(protocolid) & !is.na(type))
cn<-data.frame(colnames=tmp_type_good$colnames)
cn$num<-rep.int(1, length(cn$colnames))
b<-aggregate(.~colnames, data=cn, sum)
b$colnames<-as.factor(as.character(b$colnames))

#每个schema具体情况以及使用频率
schm<-dlply(b, .(colnames), function(d) {
  rst<-list()
  rst$detail<-splitCol(as.character(d$colnames[1]))
  rst$colname<-d$colnames[1]
  rst$num<-d$num[1]
  rst
})

#===============按照风机类型分类，找出每个类型的column并集================
#====================找到相同风场，不同风机类型==========
# fid                         types
# 1  140212        1.5MW_Switch_SSB_Hydac
# 2  140212  1.5MW_Switch_Vensys_Hydac,GL
# 3  150701     1.5MW_Freqcon_Vensys_风冷
# 4  150701  1.5MW_Switch_Vensys_Hydac,GL
# 5  520222     1.5MW_Freqcon_Vensys_风冷
# 6  520222                         2.5MW
# 7  620903     1.5MW_Freqcon_Vensys_风冷
# 8  620903                         2.5MW
# 9  620903                         3.0MW
# 10 640307     1.5MW_Freqcon_Vensys_风冷
# 11 640307  1.5MW_Switch_Vensys_Hydac,GL
# 12 652240 1.5MW_F版主动整流_Vensys_风冷
# 13 652240                         2.0MW
# 14 652240                         2.5MW
# aggregate_tmp_type<-ddply(tmp_type, .(fid), function(d){
#   l<-length(levels(as.factor(as.character(d$type))))
#   if(length(l)!=0) {
#     if(l>1) {
#       data.frame(fid=rep(d$fid[1], l), types=levels(as.factor(as.character(d$type))))
#     }
#   }
# })

#==============不同风机类型，列出所有column name，按照频率排列===========
# d<-subset(tmp_type_good, as.integer(type)==3)
type_schema_col_names<-ddply(tmp_type_good, .(type), function(d){
  colname_level<-levels(as.factor(as.character(d$colnames)))
  type_schema<-ldply(colname_level, splitCol)
  type_schema$count<-rep(1, length(type_schema$names))
  type_schema_count<-ddply(type_schema, .(names), function(td){
    l<-length(levels(as.factor(as.character(td$data_types))))
    if(length(l)!=0) {
      if(l>1) {
        data.frame(names=rep(td$names[1], l), data_types=levels(as.factor(as.character(td$data_types))))
      }
    }
  })
  type_schema_count$type<-rep(d$type[1], length(type_schema_count$names));
  write.csv(type_schema_count, paste0(resultpath, "/colname_types/", d$type[1], ".csv"), row.names = FALSE, fileEncoding="utf-8")
  
  b<-aggregate(count~names, data=type_schema, sum)
  b<-b[order(b$count, decreasing=TRUE),]
  b$type<-rep(d$type[1], length(b$names));
  write.csv(b, paste0(resultpath, "/colnames/", d$type[1], ".csv"), row.names = FALSE, fileEncoding="utf-8")
  b
})

write.csv(type_schema_col_names, paste0(resultpath, "/type_schema_col_names.csv"), row.names = FALSE)

#============对每个列，按照类型，求中文名=================



#================工具函数===========================
#分开列名和data type
# col<-as.character(tmp_type$colnames)[1]
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
  data_types<-test[seq(from=2, to=(length(test)%/%2)*2, by=2)]
  data.frame(names=to_fieldid(names), data_types=tolower(data_types))
}

to_fieldid<-function(name) {
  l<-grep("[0123456789]", substr(name, 1, 1))
  if(length(l)==0 || l!=1) {
    gsub(pattern="[^A-Za-z0-9,]", replacement="_", tolower(as.character(name)))
  } else {
    gsub(pattern="[^A-Za-z0-9,]", replacement="_", tolower(as.character(paste0("d_", name))))
  }
}


