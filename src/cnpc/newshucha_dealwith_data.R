#=========read data=============
source("newshucha_header.R")

oil_field_input<-read.csv(paste(datapath, 
                          "mid/陕京管道生产运行日报表-进气注采气/", "oil_field_input.csv", sep=""),
                          header=TRUE)
pipe_store<-read.csv(paste(datapath, 
                     "mid/陕京管道生产运行日报表-进气注采气/", "pipe_store.csv", sep=""),
                     header=TRUE)
self_used<-read.csv(paste(datapath, 
                    "mid/陕京管道生产运行日报表-进气注采气/", "self_used.csv", sep=""),
                    header=TRUE)
sub_line_input<-read.csv(paste(datapath, 
                         "mid/陕京管道生产运行日报表-进气注采气/", "sub_line_input.csv", sep=""),
                         header=TRUE)
oil_station_zhuqi<-read.csv(paste(datapath, 
                            "mid/陕京管道生产运行日报表-进气注采气/", "oil_station_zhuqi.csv", sep=""),
                            header=TRUE)
oil_station_caiqi<-read.csv(paste(datapath, 
                            "mid/陕京管道生产运行日报表-进气注采气/", "oil_station_caiqi.csv", sep=""),
                            header=TRUE)
stops_output<-read.csv(paste(datapath, 
                       "mid/陕京管道生产运行日报表-销气/", "stops_output.csv", sep=""),
                       header=TRUE)
line_self_used<-read.csv(paste(datapath, 
                         "mid/陕京输气线自耗气情况表/", "line_self_used.csv", sep=""),
                         header=TRUE)

#============filter=======================
stops_output<-subset(stops_output, line_no!="合计"&line_no!="总合计"&line_no!="大唐煤制气")
stops_output$stop_name<-na.locf(stops_output$stop_name)
stops_output<-subset(stops_output, !is.na(daily_stops_output))
stops_output<-subset(stops_output, select=-1)

write.csv(stops_output, paste(datapath, "mid/", "stops_output.csv", sep=""),row.names=FALSE)

line_self_used<-subset(line_self_used, line_type!="自耗气总合计")
line_self_used<-subset(line_self_used, gsub("([N ])", "", belongs_to)!="合计")
line_self_used<-subset(line_self_used, gsub("([N ])", "", belongs_to)!="小计")
line_self_used$line_name<-as.character(line_self_used$line_name)
line_self_used$line_name[which(is.na(line_self_used$line_name))]<-rep("noName", length(which(is.na(line_self_used$line_name))))
line_self_used<-subset(line_self_used, gsub("([N ])", "", line_name)!="小计")

write.csv(line_self_used, paste(datapath, "mid/", "line_self_used.csv", sep=""),row.names=FALSE)

oil_field_input$oil_field<-as.character(oil_field_input$oil_field)
oil_field_input$station_name<-as.character(oil_field_input$station_name)
oil_field_input<-subset(oil_field_input, grepl("合计", oil_field)==FALSE)
oil_field_input<-subset(oil_field_input, grepl("小计", station_name)==FALSE)

write.csv(oil_field_input, paste(datapath, "mid/", "oil_field_input.csv", sep=""),row.names=FALSE)

oil_station_caiqi<-subset(oil_station_caiqi, as.character(station_area)!="合计")
oil_station_caiqi<-subset(oil_station_caiqi, as.character(station_name)!="小计"&as.character(station_name)!="合计")
write.csv(oil_station_caiqi, paste(datapath, "mid/", "oil_station_caiqi.csv", sep=""),row.names=FALSE)

oil_station_zhuqi<-subset(oil_station_zhuqi, as.character(station_area)!="合计")
oil_station_zhuqi<-subset(oil_station_zhuqi, as.character(station_name)!="合计"&as.character(station_name)!="小计")
write.csv(oil_station_zhuqi, paste(datapath, "mid/", "oil_station_zhuqi.csv", sep=""),row.names=FALSE)

pipe_store<-subset(pipe_store, line!="合计")
write.csv(pipe_store, paste(datapath, "mid/", "pipe_store.csv", sep=""),row.names=FALSE)

write.csv(sub_line_input, paste(datapath, "mid/", "sub_line_input.csv", sep=""),row.names=FALSE)

#===============merge===================

