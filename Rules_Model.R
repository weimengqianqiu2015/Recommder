library(arules)
library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(plyr)
library(Tmisc)
library(tidyr)
library(reshape2)
library(methods)
Args <- commandArgs()
#####数据输入路径
in_path=Args[6]
in_file<-c(in_path)
#####数据输出路径
out_path=Args[7]
out_file<-c(out_path)
#############################################
#关联规则
Rules_Model<-function(in_file,out_file){
  xiaofeirules_duqu<-read.transactions(in_file,format="basket",sep = ",",encoding = "UTF-8")
  print("读取完毕")
  print(summary(xiaofeirules_duqu))
  print("生成规则，开始数据规整")
  xiaofeirules_rules<-apriori(xiaofeirules_duqu,parameter=list(minlen=2,maxlen=2,supp=0.0002,conf=0.01,target="rules"))
  xiaofeirules_rules<-as(xiaofeirules_rules,"data.frame")
  xiaofeirules_rules<-subset(xiaofeirules_rules,lift>1)
  xiaofeirules_rules_1<-gsub("\\{|\\}|=|>","",xiaofeirules_rules[,1])
  xiaofeirules_rules_1<-gsub("  ","$",xiaofeirules_rules_1)
  xiaofeirules_rules<-cbind(xiaofeirules_rules_1,xiaofeirules_rules[,-1])%>%data.frame()
  names(xiaofeirules_rules)[1]<-c("shop_id")
  xiaofeirules_rules<-separate(xiaofeirules_rules,shop_id,c("front_shopid","hehind_shopid"),sep = "\\$")%>%data.frame()
  xiaofeirules_rules<-xiaofeirules_rules%>%group_by(hehind_shopid)%>%arrange(desc(confidence))
  print(head(xiaofeirules_rules))
  print("数据规整完毕开始写文件")
  write.table(xiaofeirules_rules,file = out_file,sep=",",quote = FALSE,fileEncoding = "UTF-8")
  print("写文件完毕，此次任务结束")
}
Rules_Model(in_file,out_file)


