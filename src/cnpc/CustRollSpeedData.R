library(reshape2)
library(plyr)
library(plotrix)

cleandata<-function(rollSpeed) {
  rollSpeed$date<-as.Date(rollSpeed$time)
  rollSpeed$time<-as.POSIXct(rollSpeed$time)
  rollSpeed<-ddply(rollSpeed, "date", function(d){
    d[order(d$time),]
  })
  rollSpeed
}

getSlices<-function(rollSpeed, speed=1000) {
  rollSpeed$a<-rep(0, length(rollSpeed$value))
  rollSpeed$a[which(rollSpeed$value>speed)]<-1
  rollSpeed$b<-c(1, diff(rollSpeed$a))
  rollSpeed$b<-as.integer(rollSpeed$b>0)
  rollSpeed$c<-cumsum(rollSpeed$b)
  rollSpeed$pid<-rollSpeed$a*rollSpeed$c
  rollSpeed<-subset(rollSpeed, pid>0)
  if(rollSpeed$pid[1]==2){
    rollSpeed$pid<-rollSpeed$pid-1
  }
  
  result<-ddply(rollSpeed, "pid", function(d){
    m<-c(tag=as.character(d$tag[1]), 
         endTime=as.character(max(d$time)), 
         startTime=as.character(min(d$time)),
         minValue=as.numeric(max(d$value)),
         maxValue=as.numeric(max(d$value)),
         length=length(d$time), pid=d$pid[1])
    m
  })
  result
}

formatDate<-function(dateString){
  as.POSIXct(dateString)
}
plotGantt<-function(dfList, startDate, endDate, startTimeCol, endTimeCol, labelCol, gridNo=3) {
  for(filename in dfList) {
    df<-read.csv(paste0(datapath, "result/", filename), header=TRUE)
    df[[startTimeCol]]<-formatDate(df[[startTimeCol]])
    df[[endTimeCol]]<-formatDate(df[[endTimeCol]])
    df<-subset(df, df[[endTimeCol]]>startDate&df[[startTimeCol]]<endDate)
    df[[startTimeCol]][which(df[[startTimeCol]]<startDate)]<-rep(formatDate(startDate), length(which(df[[startTimeCol]]<formatDate(startDate))))
    df[[endTimeCol]][which(df[[endTimeCol]]>formatDate(endDate))]<-rep(formatDate(endDate), length(which(df[[endTimeCol]]>formatDate(endDate))))
    
    if(!exists("plotLabel")){
      plotLabel<-as.character(df[[labelCol]])
    } else {
      plotLabel<-c(plotLabel, as.character(df[[labelCol]]))
    }
    if(!exists("starts")){
      starts<-df[[startTimeCol]]
    } else {
      starts<-c(starts, df[[startTimeCol]])
    }
    if(!exists("ends")){
      ends<-df[[endTimeCol]]
    } else {
      ends<-c(ends, df[[endTimeCol]])
    }
    if(!exists("priorities")){
      priorities=rep(1, length(df[[labelCol]]))
    } else {
      priorities<-c(priorities, rep(1, length(df[[labelCol]])))
    }
  }
  
  gantt.info<-list(labels=plotLabel, 
                   starts=starts,
                   ends=ends,
                   priorities=priorities)
  
  diffDate<-as.integer(as.Date(endDate)-1-as.Date(startDate))%/%gridNo
  Ymd.format<-"%Y-%m-%d"
  vgridpos<-formatDate(strptime(as.character(as.Date(startDate)+c(0:gridNo)*diffDate), format=Ymd.format))
#   vgridpos<-c(vgridpos, formatDate(as.character(as.Date(endDate)-1)))
  vgridlab<-format(vgridpos, format = Ymd.format)
  
  mycolor <- rainbow(2, alpha=0.2)
  gantt.chart(gantt.info,
              priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,
              hgrid=TRUE, taskcolors=mycolor, label.cex=0.6)
}

#============================主调过程====================
# datapath<-"../../data/cnpc/speed/压缩机转速/"
# 
# filenames<-list.files(path=datapath, pattern=".csv")
# 
# #clean data
# for(filename in filenames) {
#   rollSpeed<-read.csv(paste0(datapath, filename), header=TRUE)
#   rollSpeed<-subset(rollSpeed, status==0)
#   
#   rollSpeed<-cleandata(rollSpeed)
#   write.csv(rollSpeed, paste0(datapath, "mid/", "s1_", filename), row.names=FALSE)
# }
# 
# #sum data
# filenames<-list.files(path=paste0(datapath, "mid"), pattern=".csv")
# for(filename in filenames) {
#   rollSpeed<-read.csv(paste0(datapath, "mid/", filename), header=TRUE)
#   rollSpeed$time<-formatDate(rollSpeed$time)
#   rollSpeed$date<-as.Date(rollSpeed$date)
#   r1000<-getSlices(rollSpeed, 1000)
#   write.csv(r1000, paste0(datapath, "result/", "slice_", filename), row.names=FALSE)
# }

mainFunction<-function(datapath, index) {
  filenames<-list.files(path=datapath, pattern=".csv")
  
  #clean data
  for(filename in filenames) {
    rollSpeed<-read.csv(paste0(datapath, filename), header=TRUE)
    rollSpeed<-subset(rollSpeed, status==0)
    
    rollSpeed<-cleandata(rollSpeed)
    write.csv(rollSpeed, paste0(datapath, "mid/", "s1_", index, filename), row.names=FALSE)
  }
  
  #sum data
  filenames<-list.files(path=paste0(datapath, "mid"), pattern=".csv")
  for(filename in filenames) {
    rollSpeed<-read.csv(paste0(datapath, "mid/", filename), header=TRUE)
    rollSpeed$time<-formatDate(rollSpeed$time)
    rollSpeed$date<-as.Date(rollSpeed$date)
    r1000<-getSlices(rollSpeed, 1000)
    write.csv(r1000, paste0(datapath, "result/", "slice_", index, filename), row.names=FALSE)
  }
}

#plot
# filenames<-list.files(path=paste0(datapath, "result"), pattern=".csv")
# 
# startDate<-"2015/10/01"
# endDate<-"2015/11/01"
# startTimeCol<-"startTime"
# endTimeCol<-"endTime"
# labelCol<-"tag"
# rm(plotLabel, starts, ends, priorities)
# 
# plotGantt(filenames, startDate, endDate, startTimeCol, endTimeCol, labelCol, gridNo=10)
