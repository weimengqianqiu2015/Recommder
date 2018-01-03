library(dplyr)
library(data.table)
library(recommenderlab)
library(Tmisc)
library(plyr)
library(tidyr)
library(proxy)
library(stringr)
library(car)
library(audio)
library(lubridate)
######################################################################
######################################################################
#十月初活动策划及数据支持
#1.业务给出店铺年消费累计金额排序
#2.业务给出店铺去年同时期活动消费累计金额排序
#3.业务给出店铺销售额拟合趋势
#4.业务给出店铺消费关联分析
#5.业务给出店铺wifi关联分析
#6.业务给出店铺忠实客户及潜在客挖掘
#7.业务给出店铺测试组输出
#####################################################################
#####################################################################
#1.业务给出店铺年消费累计金额排序
Annual_amount_range<-function(){
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  xiaofei_20160925_20170925<-fread("E:/huarun_r/xiaofeidata/xiaofei_20160925_20170925.txt",sep = "\t")
  names(xiaofei_20160925_20170925)<-c("hybh","shop_id","time","adj_amt","refund_amt")
  xiaofei_20160925_20170925_1<-xiaofei_20160925_20170925%>%mutate_at(vars(time),as.Date)%>%mutate_at(vars(adj_amt),as.numeric)%>%mutate_at(vars(refund_amt),as.numeric)
  xiaofei_20160925_20170925_1[is.na(xiaofei_20160925_20170925_1)]<-0
  xiaofei_20160925_20170925_amount<-xiaofei_20160925_20170925_1%>%group_by(shop_id)%>%dplyr::summarise(amount1=sum(adj_amt)-sum(refund_amt),num_person1=n())%>%data.frame()
  xiaofei_20160925_20170925_amount<-xiaofei_20160925_20170925_amount%>%filter(shop_id%in%shop_name$店铺编码)%>%arrange(desc(amount1))
  #barplot(xiaofei_20160925_20170925_amount$amount)
  #wait(1)
  #####################################################################
  #2.业务给出店铺去年同时期活动消费累计金额排序
  xiaofei_20160930_20161009<-subset(xiaofei_20160925_20170925_1,time<"2016-10-08"&time>"2016-10-01")
  xiaofei_20160930_20161009_1<-xiaofei_20160930_20161009%>%filter(shop_id%in%shop_name$店铺编码)
  xiaofei_20160930_20161009_1_amount<-xiaofei_20160930_20161009_1%>%group_by(shop_id)%>%
    dplyr::summarise(amount2=sum(adj_amt)-sum(refund_amt),num_person2=n())%>%
    data.frame()%>%arrange(desc(amount2))
  #同时期销售额占全年比例
  shop_amount_two<-left_join(xiaofei_20160925_20170925_amount,xiaofei_20160930_20161009_1_amount,by="shop_id")
  shop_amount_two<-shop_amount_two%>%mutate(per_amt=amount2/amount1,per_unmp=num_person2/num_person1)
  #####################################################################
  
  return(shop_amount_two)
}
Annual_amount_range_data<-Annual_amount_range()
#####################################################################
#####################################################################
#3.业务给出店铺销售额拟合趋势,取一年数据，五个与数据，三个月数据
Otc_shop_id_201608_201709<-function(){
  ##shop_name
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  xiaofei_20160925_20170925<-fread("E:/huarun_r/xiaofeidata/xiaofei_20160925_20170925.txt",sep = "\t")
  names(xiaofei_20160925_20170925)<-c("hybh","shop_id","time","adj_amt","refund_amt")
  xiaofei_20160925_20170925_1<-xiaofei_20160925_20170925%>%mutate_at(vars(time),as.Date)%>%mutate_at(vars(adj_amt),as.numeric)%>%mutate_at(vars(refund_amt),as.numeric)
  xiaofei_20160925_20170925_1[is.na(xiaofei_20160925_20170925_1)]<-0
  xiaofei_20160930_20161009<-subset(xiaofei_20160925_20170925_1,time<"2017-09-1"&time>"2016-09-30")
  xiaofei_20160930_20161009_1<-xiaofei_20160930_20161009%>%filter(shop_id%in%shop_name$店铺编码)
  xiaofei_20160930_20161009_2<-xiaofei_20160930_20161009_1%>%group_by(shop_id,month=month(time))%>%
    dplyr::summarise(amount=sum(adj_amt,na.rm = TRUE)-sum(refund_amt,na.rm = TRUE),count=n())%>%data.frame()
  otc_monthly_sales_4<-xiaofei_20160930_20161009_2
  xiaofei_20160930_20161009_3<-xiaofei_20160930_20161009_2%>%group_by(shop_id)%>%dplyr::summarise(count=n())%>%filter(count>6)%>%data.frame()%>%select(shop_id)
  shop_name_2<-xiaofei_20160930_20161009_3$shop_id
  v_name<-list()
  for(i in 1:length(shop_name_2)){
    otc_monthly_sales_2_shop<-otc_monthly_sales_4%>%filter(shop_id%in%shop_name_2[i])
    print(otc_monthly_sales_2_shop)
    #otc_monthly_sales_2_shop$range<-seq(1:dim(otc_monthly_sales_2_shop)[1])
    otc_monthly_sales_3<-otc_monthly_sales_2_shop[,c(2,3)]
    m<-scatterplot(amount~month,otc_monthly_sales_3,xlab = paste0(shop_name_2[i],"month"),ylab =paste0(shop_name_2[i], "amount"))
    modle<-lm(amount~month+1,data=otc_monthly_sales_3)
    abline(modle)
    if(coef(modle)[2]>1000){
      v_name<-append(v_name,shop_name_2[i])
      print(shop_name_2[i])
      print(v_name)
    }
    wait(1)
    print(i)
  }
  v_name_1<-unlist(v_name)
  return(v_name_1)
}
Otc_shop_id_201608_201709_data<-Otc_shop_id_201608_201709()
Otc_shop_id_201608_201709_data_frame<-data.frame(shop_id=(Otc_shop_id_201608_201709_data),year_index=rep(1,length(Otc_shop_id_201608_201709_data)))
Otc_shop_id_201704_201709<-function(){
  ##shop_name
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  xiaofei_20160925_20170925<-fread("E:/huarun_r/xiaofeidata/xiaofei_20160925_20170925.txt",sep = "\t")
  names(xiaofei_20160925_20170925)<-c("hybh","shop_id","time","adj_amt","refund_amt")
  xiaofei_20160925_20170925_1<-xiaofei_20160925_20170925%>%mutate_at(vars(time),as.Date)%>%mutate_at(vars(adj_amt),as.numeric)%>%mutate_at(vars(refund_amt),as.numeric)
  xiaofei_20160925_20170925_1[is.na(xiaofei_20160925_20170925_1)]<-0
  xiaofei_20160930_20161009<-subset(xiaofei_20160925_20170925_1,time<"2017-09-1"&time>"2017-03-31")
  xiaofei_20160930_20161009_1<-xiaofei_20160930_20161009%>%filter(shop_id%in%shop_name$店铺编码)
  xiaofei_20160930_20161009_2<-xiaofei_20160930_20161009_1%>%group_by(shop_id,month=month(time))%>%
    dplyr::summarise(amount=sum(adj_amt,na.rm = TRUE)-sum(refund_amt,na.rm = TRUE),count=n())%>%data.frame()
  otc_monthly_sales_4<-xiaofei_20160930_20161009_2
  xiaofei_20160930_20161009_3<-xiaofei_20160930_20161009_2%>%group_by(shop_id)%>%dplyr::summarise(count=n())%>%filter(count>3)%>%data.frame()%>%select(shop_id)
  shop_name_2<-xiaofei_20160930_20161009_3$shop_id
  v_name<-list()
  for(i in 1:length(shop_name_2)){
    otc_monthly_sales_2_shop<-otc_monthly_sales_4%>%filter(shop_id%in%shop_name_2[i])
    print(otc_monthly_sales_2_shop)
    #otc_monthly_sales_2_shop$range<-seq(1:dim(otc_monthly_sales_2_shop)[1])
    otc_monthly_sales_3<-otc_monthly_sales_2_shop[,c(2,3)]
    m<-scatterplot(amount~month,otc_monthly_sales_3,xlab = paste0(shop_name_2[i],"month"),ylab =paste0(shop_name_2[i], "amount"))
    modle<-lm(amount~month+1,data=otc_monthly_sales_3)
    abline(modle)
    if(coef(modle)[2]>1000){
      v_name<-append(v_name,shop_name_2[i])
      print(shop_name_2[i])
      print(v_name)
    }
    wait(1)
    print(i)
  }
  v_name_1<-unlist(v_name)
  return(v_name_1)
}
Otc_shop_id_201704_201709_data<-Otc_shop_id_201704_201709()
Otc_shop_id_201704_201709_data_frame<-data.frame(shop_id=(Otc_shop_id_201704_201709_data),halfyear_index=rep(1,length(Otc_shop_id_201704_201709_data)))
Otc_shop_id_201706_201709<-function(){
  ##shop_name
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  xiaofei_20160925_20170925<-fread("E:/huarun_r/xiaofeidata/xiaofei_20160925_20170925.txt",sep = "\t")
  names(xiaofei_20160925_20170925)<-c("hybh","shop_id","time","adj_amt","refund_amt")
  xiaofei_20160925_20170925_1<-xiaofei_20160925_20170925%>%mutate_at(vars(time),as.Date)%>%mutate_at(vars(adj_amt),as.numeric)%>%mutate_at(vars(refund_amt),as.numeric)
  xiaofei_20160925_20170925_1[is.na(xiaofei_20160925_20170925_1)]<-0
  xiaofei_20160930_20161009<-subset(xiaofei_20160925_20170925_1,time<"2017-09-01"&time>"2017-05-31")
  xiaofei_20160930_20161009_1<-xiaofei_20160930_20161009%>%filter(shop_id%in%shop_name$店铺编码)
  xiaofei_20160930_20161009_2<-xiaofei_20160930_20161009_1%>%group_by(shop_id,month=month(time))%>%
    dplyr::summarise(amount=sum(adj_amt,na.rm = TRUE)-sum(refund_amt,na.rm = TRUE),count=n())%>%data.frame()
  otc_monthly_sales_4<-xiaofei_20160930_20161009_2
  xiaofei_20160930_20161009_3<-xiaofei_20160930_20161009_2%>%group_by(shop_id)%>%dplyr::summarise(count=n())%>%filter(count>1)%>%data.frame()%>%select(shop_id)
  shop_name_2<-xiaofei_20160930_20161009_3$shop_id
  v_name<-list()
  for(i in 1:length(shop_name_2)){
    otc_monthly_sales_2_shop<-otc_monthly_sales_4%>%filter(shop_id%in%shop_name_2[i])
    print(otc_monthly_sales_2_shop)
    #otc_monthly_sales_2_shop$range<-seq(1:dim(otc_monthly_sales_2_shop)[1])
    otc_monthly_sales_3<-otc_monthly_sales_2_shop[,c(2,3)]
    m<-scatterplot(amount~month,otc_monthly_sales_3,xlab = paste0(shop_name_2[i],"month"),ylab =paste0(shop_name_2[i], "amount"))
    modle<-lm(amount~month+1,data=otc_monthly_sales_3)
    abline(modle)
    if(coef(modle)[2]>1000){
      v_name<-append(v_name,shop_name_2[i])
      print(shop_name_2[i])
      print(v_name)
    }
    wait(1)
    print(i)
  }
  v_name_1<-unlist(v_name)
  return(v_name_1)
}
Otc_shop_id_201706_201709_data<-Otc_shop_id_201706_201709()
Otc_shop_id_201706_201709_data_frame<-data.frame(shop_id=(Otc_shop_id_201706_201709_data),three_index=rep(1,length(Otc_shop_id_201706_201709_data)))
#取交集
temp_shop_id<-intersect(Otc_shop_id_201608_201709_data,Otc_shop_id_201704_201709_data)
increase_shop_id<-intersect(temp_shop_id,Otc_shop_id_201706_201709_data)
#取交集后趋势展示
#increase_shop_id_test<-function(increase_shop_id){
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  otc_monthly_sales_2<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/Otc_shop_amount.txt")%>%data.frame()
  otc_monthly_sales_4<-otc_monthly_sales_2%>%filter(V2%in%increase_shop_id)%>%data.frame()
  #otc_monthly_sales_5<-otc_monthly_sales_4%>%group_by(V2)%>%dplyr::summarise(count=n())%>%arrange(count)%>%filter(count==1)%>%select(V2)
  otc_monthly_sales_5<-unique(otc_monthly_sales_4$V2)
  #shop_name_1<-intersect(unique(otc_monthly_sales_2$V2),shop_name$店铺编码)
  #shop_name_2<-setdiff(shop_name_1,otc_monthly_sales_5$V2)
  #shop_name_1_lack<-setdiff(shop_name$店铺编码,shop_name_1)
  #write.table(shop_name_1_lack,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/shop_name_1_lack.txt",quote=FALSE,row.names=FALSE)
  v_name<-list()
  for(i in 1:length(otc_monthly_sales_5)){
    #otc_monthly_sales_2_shop<-otc_monthly_sales_4[,i]
    otc_monthly_sales_2_shop<-otc_monthly_sales_4%>%filter(V2%in%otc_monthly_sales_5[i])
    print(otc_monthly_sales_2_shop)
    otc_monthly_sales_2_shop$range<-seq(1:dim(otc_monthly_sales_2_shop)[1])
    otc_monthly_sales_3<-otc_monthly_sales_2_shop[,c(4,3)]
    m<-scatterplot(V3~range,otc_monthly_sales_3,xlab = paste0(increase_shop_id[i],"month"),ylab =paste0(increase_shop_id[i], "amount"))
    modle<-lm(V3~range+1,data=otc_monthly_sales_3)
    abline(modle)
    if(coef(modle)[2]>10){
      v_name<-append(v_name,otc_monthly_sales_5[i])
      print(otc_monthly_sales_5[i])
      print(v_name)
    }
    wait(1)
    print(i)
  }
  v_name_1<-unlist(v_name)
  return(v_name_1)
}
####################################################################
####################################################################
#4.业务给出店铺消费关联分析,按照前项出现次数和提升度制作关联消费指数并排序
xiaofeirules_rules_all<-function(input_path,output_path){
  input_path<-c("E:/huarun_r/temp_need/华润活动店铺需求/10月份/xiaofei_20160730_20170901.txt")
  output_path<-c("E:/huarun_r/temp_need/华润活动店铺需求/10月份/guanlian_xiaofei_20160901_20170921.txt")
  xiaofeirules_duqu<-read.transactions(input_path,format="single",cols=c(1,2),sep = "\t",encoding = "UTF-8")
  print("读取完毕")
  print(summary(xiaofeirules_duqu))
  #barplot(sort(itemFrequency(xiaofeirules_duqu),decreasing = TRUE)[1:20])
  print("生成规则")
  xiaofeirules_rules<-apriori(xiaofeirules_duqu,parameter=list(minlen=2,maxlen=2,supp=0.0003,conf=0.2,target="rules"))
  #summary(xiaofeirules_rules)
  xiaofeirules_rules<-as(xiaofeirules_rules,"data.frame")
  xiaofeirules_rules<-subset(xiaofeirules_rules,lift>1)
  xiaofeirules_rules_1<-gsub("=>","$",xiaofeirules_rules[,1])
  xiaofeirules_rules_1<-gsub("\\{|\\}| ","",xiaofeirules_rules_1)
  xiaofeirules_rules<-cbind(xiaofeirules_rules_1,xiaofeirules_rules[,-1])
  xiaofeirules_rules<-separate(xiaofeirules_rules,xiaofeirules_rules_1,c("shopid_qian","shopid_hou"),sep = "\\$")
  print("写文件")
  write.table(xiaofeirules_rules,file = output_path,quote = FALSE,fileEncoding = "UTF-8",row.names = FALSE)
  print("处理完毕")
  return(xiaofeirules_rules)
}
xiaofeirules_rules_all_1<-xiaofeirules_rules_all(input_path,output_path)
guanlian_xiaofei_index<-function(xiaofeirules_rules_all_1){
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  xiaofeirules_rules_all_2<-xiaofeirules_rules_all_1%>%arrange(desc(lift))
  #直接从消费关联规则中筛选
  #前项筛选
  z_score<-function(x){
    (x-min(x))/(max(x)-min(x))
  }
  xiaofeirules_rules_all_3<-xiaofeirules_rules_all_1%>%filter(shopid_qian%in%shop_name$店铺编码)
  xiaofeirules_rules_all_4<-xiaofeirules_rules_all_3%>%group_by(shopid_qian)%>%
    dplyr::summarise(count=n(),qian_lift_min=min(lift),qian_lift_max=max(lift),
                     qian_lift_median=median(lift))%>%arrange(desc(qian_lift_max))%>%data.frame()
  xiaofeirules_rules_all_4_1<-xiaofeirules_rules_all_4
  for(i in 2:dim(xiaofeirules_rules_all_4_1)[2]){
    xiaofeirules_rules_all_4_1[,i]<-z_score(xiaofeirules_rules_all_4_1[,i]) 
  }
  xiaofeirules_rules_all_4_2<-mutate(xiaofeirules_rules_all_4_1,
                                     index=0.5*count+0.3*qian_lift_max+0.2*qian_lift_median)
  xiaofeirules_rules_all_4_3<-inner_join(xiaofeirules_rules_all_4,xiaofeirules_rules_all_4_2,by="shopid_qian")%>%arrange(desc(index))
  xiaofeirules_rules_all_4_4<-xiaofeirules_rules_all_4_3[,c(1,2,4,5,10)]
  names(xiaofeirules_rules_all_4_4)<-c("shop_id","xiaofei_count","xiaofei_max","xiaofei_median","xiaofei_index")
  #write.table(xiaofeirules_rules_all_4_4,file = "E:/xiaofeirules_rules_all_4_4.txt",row.names = FALSE,quote = FALSE)
  return(xiaofeirules_rules_all_4_4)
}
guanlian_xiaofei_index_data<-guanlian_xiaofei_index(xiaofeirules_rules_all_1)
####################################################################
####################################################################
#5.业务给出店铺wifi关联分析,按照前项出现次数和提升度制作关联wifi指数并排序
wifirules_rules_all<-function(input_path,output_path){
  input_path<-c("E:/huarun_r/wifi/wifi_kaibo_2.txt")
  output_path<-c("E:/huarun_r/wifi/wifi_kaibo_guanlian.txt")
  xiaofeirules_duqu<-read.transactions(input_path,format="single",cols=c(1,2),sep = ",",encoding = "UTF-8")
  print("读取完毕")
  print(summary(xiaofeirules_duqu))
  #barplot(sort(itemFrequency(xiaofeirules_duqu),decreasing = TRUE)[1:20])
  print("生成规则")
  xiaofeirules_rules<-apriori(xiaofeirules_duqu,parameter=list(minlen=2,maxlen=2,supp=0.001,conf=0.3,target="rules"))
  #summary(xiaofeirules_rules)
  xiaofeirules_rules<-as(xiaofeirules_rules,"data.frame")
  xiaofeirules_rules<-subset(xiaofeirules_rules,lift>1)
  xiaofeirules_rules_1<-gsub("=>","$",xiaofeirules_rules[,1])
  xiaofeirules_rules_1<-gsub("\\{|\\}| ","",xiaofeirules_rules_1)
  xiaofeirules_rules<-cbind(xiaofeirules_rules_1,xiaofeirules_rules[,-1])
  xiaofeirules_rules<-separate(xiaofeirules_rules,xiaofeirules_rules_1,c("shopid_qian","shopid_hou"),sep = "\\$")
  print("写文件")
  write.table(xiaofeirules_rules,file = output_path,quote = FALSE,fileEncoding = "UTF-8",row.names = FALSE)
  print("处理完毕")
  return(xiaofeirules_rules)
}
wifirules_rules_all_1<-wifirules_rules_all(input_path,output_path)
guanlian_wifi_index<-function(wifirules_rules_all_1){
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  
  wifirules_rules_all_2<-wifirules_rules_all_1%>%arrange(desc(lift))
  #直接从消费关联规则中筛选
  #前项筛选
  z_score<-function(x){
    (x-min(x))/(max(x)-min(x))
  }
  wifirules_rules_all_3<-wifirules_rules_all_2%>%filter(shopid_qian%in%shop_name$店铺编码)
  wifirules_rules_all_4<-wifirules_rules_all_3%>%group_by(shopid_qian)%>%
    dplyr::summarise(count=n(),qian_lift_min=min(lift),qian_lift_max=max(lift),
                     qian_lift_median=median(lift))%>%arrange(desc(qian_lift_max))%>%data.frame()
  wifirules_rules_all_4_1<-wifirules_rules_all_4
  for(i in 2:dim(wifirules_rules_all_4_1)[2]){
    wifirules_rules_all_4_1[,i]<-z_score(wifirules_rules_all_4_1[,i]) 
  }
  wifirules_rules_all_4_2<-mutate(wifirules_rules_all_4_1,
                                  index=0.5*count+0.3*qian_lift_max+0.2*qian_lift_median)
  wifirules_rules_all_4_3<-inner_join(wifirules_rules_all_4,wifirules_rules_all_4_2,by="shopid_qian")%>%arrange(desc(index))
  wifirules_rules_all_4_4<-wifirules_rules_all_4_3[,c(1,2,4,5,10)]
  names(wifirules_rules_all_4_4)<-c("shop_id","wifi_count","wifi_max","wifi_median","wifi_index")
  
  #write.table(wifirules_rules_all_4_4,file = "E:/wifirules_rules_all_4_4.txt",row.names = FALSE,quote = FALSE)
  return(wifirules_rules_all_4_4)
}
guanlian_wifi_index_data<-guanlian_wifi_index(wifirules_rules_all_1)
####################################################################
####################################################################
#指标汇总
all_shop_year_amount<-function(){
  shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
  xiaofei_20160925_20170925<-fread("E:/huarun_r/xiaofeidata/xiaofei_20160925_20170925.txt",sep = "\t")
  names(xiaofei_20160925_20170925)<-c("hybh","shop_id","time","adj_amt","refund_amt")
  xiaofei_20160925_20170925_1<-xiaofei_20160925_20170925%>%mutate_at(vars(time),as.Date)%>%mutate_at(vars(adj_amt),as.numeric)%>%mutate_at(vars(refund_amt),as.numeric)
  xiaofei_20160925_20170925_1[is.na(xiaofei_20160925_20170925_1)]<-0
  xiaofei_20160925_20170925_amount<-xiaofei_20160925_20170925_1%>%group_by(shop_id)%>%dplyr::summarise(amount1=sum(adj_amt)-sum(refund_amt),num_person1=n())%>%data.frame()
  xiaofei_20160925_20170925_amount<-xiaofei_20160925_20170925_amount%>%filter(shop_id%in%shop_name$店铺编码)%>%arrange(desc(amount1))
  return(xiaofei_20160925_20170925_amount)
}
all_shop_year_amount_data<-all_shop_year_amount()
temp_2<-full_join(Annual_amount_range_data,guanlian_xiaofei_index_data,by="shop_id")
temp_3<-full_join(temp_2,guanlian_wifi_index_data,by="shop_id")
temp_4<-full_join(temp_3,Otc_shop_id_201608_201709_data_frame,by="shop_id")
temp_5<-full_join(temp_4,Otc_shop_id_201704_201709_data_frame,by="shop_id")
temp_6<-full_join(temp_5,Otc_shop_id_201706_201709_data_frame,by="shop_id")
temp_6[is.na(temp_6)]<--1
#定义潜力店铺
qianligu<-subset(temp_6,amount1>=median(amount1)|amount2>=median(amount2))
#高收入且趋势好
leader_1<-subset(qianligu,amount1>=median(amount1)&amount2>median(amount2)&year_index==1&
                   halfyear_index==1&
                   three_index==1)
#高收入且趋势良
qianligu_2<-qianligu%>%filter(!shop_id%in%leader_1$shop_id)
leader_2<-subset(qianligu_2,amount1>=median(amount1)&amount2>median(amount2)&three_index==1)
#高收入且群组性强
qianligu_3<-qianligu_2%>%filter(!shop_id%in%leader_1$shop_id&!shop_id%in%leader_2$shop_id)
leader_3<-subset(qianligu_3,amount1>=median(amount1)&amount2>median(amount2)&xiaofei_count>1&
                   wifi_count>1)
#高收入且引流性强
qianligu_4<-qianligu_3%>%filter(!shop_id%in%leader_1$shop_id&!shop_id%in%leader_2$shop_id&!shop_id%in%leader_3$shop_id)
leader_4<-subset(qianligu_4,amount1>=median(amount1)&amount2>median(amount2)&num_person2>52)
####################################################################
####################################################################
#为种子店铺筛选关联店铺
rules_all<-rbind(xiaofeirules_rules_all_1,wifirules_rules_all_1)
rules_all_1<-rules_all[,c(1,2)]%>%unique()
rules_all_1<-rules_all_1%>%filter(shopid_qian%in%shop_name$店铺编码&shopid_hou%in%shop_name$店铺编码)
#leader_1_result
leader_1_result<-function(rules_all_1,leader){
  leader_1_shop<-rules_all_1%>%filter(shopid_qian%in%leader$shop_id)%>%arrange(desc(shopid_qian))
  leader_1_shop<-leader_1_shop[,c(2,1)]
  leader_1_shop_hou<-leader_1_shop$shopid_hou%>%unique()%>%data.frame()
  names(leader_1_shop_hou)<-c("shopid_hou")
  leader_1_shop_hou$class<-1:rep(1:dim(leader_1_shop_hou)[1])
  x<-plyr::join(leader_1_shop_hou,leader_1_shop,by = intersect(names(leader_1_shop_hou), names(leader_1_shop)),
                type = "left", match = "first")
  y<-x[,c(3,1)]
  return(y)
}
leader_1_result_data<-leader_1_result(rules_all_1,leader_1)
leader_1_result_data<-leader_1_result_data%>%filter(shopid_hou%in%shop_name$店铺编码)
write.table(leader_1_result_data,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/leader_1.csv",sep=",",row.names=FALSE,quote=FALSE)
#leader_2_result
leader_2_result_data<-leader_1_result(rules_all_1,leader_2)
leader_2_result_data_1<-leader_2_result_data%>%filter(!shopid_hou%in%leader_1_result_data$shopid_hou)
write.table(leader_2_result_data,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/leader_2.csv",sep=",",row.names=FALSE,quote=FALSE)

#leader_3_result
leader_3_result_data<-leader_1_result(rules_all_1,leader_3)
write.table(leader_3_result_data,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/leader_3.csv",sep=",",row.names=FALSE,quote=FALSE)

leader_3_result_data_1<-leader_3_result_data%>%filter(!shopid_hou%in%leader_1_result_data$shopid_hou&
                                                        !shopid_hou%in%leader_2_result_data_1$shopid_hou)

#leader_4_result
leader_4_result_data<-leader_1_result(rules_all_1,leader_4)
write.table(leader_4_result_data,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/leader_4.csv",sep=",",row.names=FALSE,quote=FALSE)

leader_4_result_data_1<-leader_4_result_data%>%filter(!shopid_hou%in%leader_1_result_data$shopid_hou&
                                                        !shopid_hou%in%leader_2_result_data_1$shopid_hou&
                                                        !shopid_hou%in%leader_3_result_data_1)


x<-list(leader_1_result_data,leader_2_result_data,leader_3_result_data,leader_4_result_data)
y1<-union(leader_1_result_data$shopid_qian,leader_1_result_data$shopid_hou)
y2<-union(leader_2_result_data$shopid_qian,leader_2_result_data$shopid_hou)
y3<-union(leader_3_result_data$shopid_qian,leader_3_result_data$shopid_hou)
y4<-union(leader_4_result_data$shopid_qian,leader_4_result_data$shopid_hou)
m1<-union(y1,y2)
m2<-union(m1,y3)
m3<-union(m2,y4)
length(unique(m3))
#最终店铺,第一组
shop_id_1<-c("L27601",
             "L27602",
             "L0206N03",
             "L0208N03",
             "L36001",
             "L0358N02",
             "L0498N01"
)

shop_id_2<-c("L37002",
             "L0216N01",
             "L0439N01",
             "L0478N02",
             "L0480N01",
             "L0355N01"
             
)

shop_id_3<-c("L69602",
             "L67201",
             "L0607N01",
             "L0603N01",
             "L0680N01",
             "L0596N01"
             
)
shop_id_4<-c(
             "L29003",
             "L0292N01",
             "L0323N03",
             "L0321N02"
)

x<-xiaofei_20160925_20170925_1%>%filter(shop_id%in%shop_id_1&time>="2016-10-01"&time<="2016-10-07")%>%summarise(amount=sum(adj_amt)-sum(refund_amt))
1293526
637221.8
587134.2
397695
####################################################################
####################################################################
#6.业务给出店铺忠实客户
xiaofei_20160925_20170925<-fread("E:/huarun_r/xiaofeidata/xiaofei_20160925_20170925.txt",sep = "\t")%>%data.frame()
xiaofei_20160925_20170925_1<-xiaofei_20160925_20170925[,c(2,1)]
names(xiaofei_20160925_20170925_1)<-c("shop_id","hybh")
loyal_shop_id_1<-xiaofei_20160925_20170925_1%>%filter(shop_id%in%shop_id_1)
loyal_shop_id_2<-xiaofei_20160925_20170925_1%>%filter(shop_id%in%shop_id_2)
loyal_shop_id_3<-xiaofei_20160925_20170925_1%>%filter(shop_id%in%shop_id_3)
loyal_shop_id_4<-xiaofei_20160925_20170925_1%>%filter(shop_id%in%shop_id_4)
##写忠实客户
write.table(unique(loyal_shop_id_1$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第一组_老客.txt",row.names=FALSE,quote=FALSE)
write.table(unique(loyal_shop_id_2$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第二组_老客.txt",row.names=FALSE,quote=FALSE)
write.table(unique(loyal_shop_id_3$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第三组_老客.txt",row.names=FALSE,quote=FALSE)
write.table(unique(loyal_shop_id_4$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第四组_老客.txt",row.names=FALSE,quote=FALSE)

#shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
#loyal<-xiaofei_20160925_20170925%>%filter(V2%in%shop_name$店铺编码)
####################################################################
####################################################################
#7.业务给出店铺潜在客
qianke_final<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/qianke_ally_final_result.txt",sep=",",header=FALSE)%>%data.frame()
qianke_final_1<-qianke_final[,c(1,2)]
names(qianke_final_1)<-c("shop_id","hybh")
qianke_shop_id_1<-qianke_final_1%>%filter(shop_id%in%shop_id_1)
unique(qianke_shop_id_1$hybh)
qianke_shop_id_2<-qianke_final_1%>%filter(shop_id%in%shop_id_2)
qianke_shop_id_3<-qianke_final_1%>%filter(shop_id%in%shop_id_3)
qianke_shop_id_4<-qianke_final_1%>%filter(shop_id%in%shop_id_4)
#写潜客
write.table(unique(qianke_shop_id_1$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第一组_潜客.txt",row.names=FALSE,quote=FALSE)
write.table(unique(qianke_shop_id_2$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第二组_潜客.txt",row.names=FALSE,quote=FALSE)
write.table(unique(qianke_shop_id_3$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第三组_潜客.txt",row.names=FALSE,quote=FALSE)
write.table(unique(qianke_shop_id_4$hybh),file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第四组_潜客.txt",row.names=FALSE,quote=FALSE)

#qianke_shop_id_5<-qianke_final_1%>%filter(shop_id%in%shop_id_5)
#qianke_shop_id_intersect_1<-qianke_final_1%>%filter(shop_id%in%"L0323N03")
#qianke_shop_id_intersect_2<-qianke_final_1%>%filter(shop_id%in%"L31802")
#整合忠实客户和潜在客户
#第一组
object_1<-rbind(loyal_shop_id_1,qianke_shop_id_1)
object_1_result<-unique(object_1$hybh)
length(object_1_result)
#第二组
object_2<-rbind(loyal_shop_id_2,qianke_shop_id_2)
object_2_result<-setdiff(unique(object_2$hybh),object_1_result)
length(object_2_result)
#第四组
object_4<-rbind(loyal_shop_id_4,qianke_shop_id_4)
temp<-union(object_2_result,object_1_result)
object_4_result<-setdiff(object_4$hybh,temp)
length(object_4_result)
#第三组
object_3<-rbind(loyal_shop_id_3,qianke_shop_id_3)
temp_1<-union(object_4_result,temp)
object_3_result<-setdiff(object_3$hybh,temp_1)
length(object_3_result)
##写文件
write.table(object_1_result,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第一组.txt",row.names=FALSE,quote=FALSE)
write.table(object_2_result,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第二组.txt",row.names=FALSE,quote=FALSE)
write.table(object_3_result,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第三组.txt",row.names=FALSE,quote=FALSE)
write.table(object_4_result,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第四组.txt",row.names=FALSE,quote=FALSE)

##检查
m<-union(object_1_result,object_2_result)
intersect(object_3_result,object_4_result)
m1<-union(object_4_result,m)
m2<-union(object_3_result,m1)
# ############
# #ABtest
# ABtest_rfm<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺对比店铺/rfm_ABtest.txt")
# names(ABtest_rfm)<-c("hybh","class")
# object_all<-data.frame(hybh=m2,qianke=rep(1,length(m2)))
# object_all_rfm<-left_join(ABtest_rfm,object_all,by="hybh")
# object_all_rfm[is.na(object_all_rfm)]<-0
# #B人群
# B_group<-subset(object_all_rfm,qianke==0)
# table(B_group$class)
# #选出object_1_result人群RFM模型分布
# qianke_shop_id_1_hybh<-unique(qianke_shop_id_1$hybh)
# qianke_shop_id_1_frm<-object_all_rfm%>%filter(hybh%in%qianke_shop_id_1_hybh)
# table(qianke_shop_id_1_frm$class)
# #选出object_2_result人群RFM模型分布
# qianke_shop_id_2_hybh<-unique(qianke_shop_id_2$hybh)
# qianke_shop_id_2_frm<-object_all_rfm%>%filter(hybh%in%qianke_shop_id_2_hybh)
# table(qianke_shop_id_2_frm$class)
# #选出object_3_result人群RFM模型分布
# qianke_shop_id_3_hybh<-unique(qianke_shop_id_3$hybh)
# qianke_shop_id_3_frm<-object_all_rfm%>%filter(hybh%in%qianke_shop_id_3_hybh)
# table(qianke_shop_id_3_frm$class)
# #选出object_4_result人群RFM模型分布
# qianke_shop_id_4_hybh<-unique(qianke_shop_id_4$hybh)
# qianke_shop_id_4_frm<-object_all_rfm%>%filter(hybh%in%qianke_shop_id_4_hybh)
# table(qianke_shop_id_4_frm$class)
####激活人群
xiaofei_20160924_20150924<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/激活人群/xiaofei_20160924_20150924.txt")
names(xiaofei_20160924_20150924)<-c("hybh","shop_id","time","adj_amt","refund_amt")
xiaofei_20160924_20150924_1<-xiaofei_20160924_20150924%>%mutate_at(vars(time),as.Date)%>%mutate_at(vars(adj_amt),as.numeric)%>%mutate_at(vars(refund_amt),as.numeric)
xiaofei_20160924_20150924_1[is.na(xiaofei_20160924_20150924_1)]<-0
shop_id_all<-c("L27601",
               "L27602",
               "L0206N03",
               "L0208N03",
               "L36001",
               "L0358N02",
               "L0498N01",
               "L37002",
               "L0216N01",
               "L0439N01",
               "L0478N02",
               "L0480N01",
               "L0355N01",
               "L69602",
               "L67201",
               "L0607N01",
               "L0603N01",
               "L0680N01",
               "L0596N01",
               "L29003",
               "L0292N01",
               "L0323N03",
               "L0321N02"
               )
xiaofei_20160924_20150924_2<-xiaofei_20160924_20150924_1%>%filter(shop_id%in%shop_id_all)
ahop_all_2<-unique(xiaofei_20160924_20150924_2$shop_id)

#十月活动目标人群
object_all<-data.frame(hybh=m2,qianke=rep(1,length(m2)))
#
ABtest_rfm<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺对比店铺/rfm_ABtest.txt")
names(ABtest_rfm)<-c("hybh","class")
#
lest<-setdiff(xiaofei_20160924_20150924_2$hybh,xiaofei_20160924_20170927_hybh)
xiaofei_20160924_20150924_3<-xiaofei_20160924_20150924_2%>%filter(hybh%in%lest)
shop_id_1_jihuo<-xiaofei_20160924_20150924_3%>%filter(shop_id%in%shop_id_1)
shop_id_1_jihuo_hybh<-unique(shop_id_1_jihuo$hybh)
shop_id_2_jihuo<-xiaofei_20160924_20150924_3%>%filter(shop_id%in%shop_id_2)
shop_id_2_jihuo_hybh<-unique(shop_id_2_jihuo$hybh)
shop_id_3_jihuo<-xiaofei_20160924_20150924_3%>%filter(shop_id%in%shop_id_3)
shop_id_3_jihuo_hybh<-unique(shop_id_3_jihuo$hybh)
shop_id_4_jihuo<-xiaofei_20160924_20150924_3%>%filter(shop_id%in%shop_id_4)
shop_id_4_jihuo_hybh<-unique(shop_id_4_jihuo$hybh)
write.table(shop_id_1_jihuo_hybh,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第一组激活人群.txt",row.names=FALSE,quote=FALSE)
write.table(shop_id_2_jihuo_hybh,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第二组激活人群.txt",row.names=FALSE,quote=FALSE)
write.table(shop_id_3_jihuo_hybh,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第三组激活人群.txt",row.names=FALSE,quote=FALSE)
write.table(shop_id_4_jihuo_hybh,file = "E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/十月活动店铺组目标人群/Otc_第四组激活人群.txt",row.names=FALSE,quote=FALSE)

#
xiaofei_20160924_20170927<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/店铺筛选/激活人群/xiaofei_20160924_20170927.txt")
xiaofei_20160924_20170927_hybh<-unique(xiaofei_20160924_20170927$mem_num)
intersect(lest_1,xiaofei_20160924_20170927_hybh)
#shop_name<-fread("E:/huarun_r/temp_need/华润活动店铺需求/10月份/国庆节参与店铺号.txt",sep="\n",header=TRUE)%>%data.frame()
#qianke_sub<-qianke_final%>%filter(V1%in%shop_name$店铺编码)
####################################################################
####################################################################
#8.业务给出店铺测试组输出

