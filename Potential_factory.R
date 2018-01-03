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
library(recommenderlab)
library(proxy)
#此脚本旨在打通潜客挖掘各个环节，使之潜客挖掘各部分脚本合并在一起方便使用
#需要的字段有：店铺号，会员编号，消费时间，消费金额，退款金额
#关联规则需要的字段为：店铺号，会员编号，消费时间
#关联规则需要六出的接口为消费时间窗口、事务型数据的时间窗口、支持度和置信度的阈值预留
#Rules_time_start,Rules_time_end,Rules_transaction_type,Rules_souppt,Rules_confident
#####################################################################################
#####################################################################################
#读取消费数据，一共五个字段店铺号，会员编号，消费时间，消费金额，退款金额
consum_data<-fread("E:/徐博士临时需求/典型商家数据会员消费明细20171208.csv")%>%data.frame()%>%
  select(商家名称,订单时间,商品标识,商品名称,商品总金额,会员编号)%>%data.frame()
consum_data_1<-na.omit(consum_data)
consum_data_2<-subset(consum_data_1,商品总金额>10)
summary<-fread("E:/徐博士临时需求/summary.csv")%>%data.frame()
summary<-summary%>%as.vector()
# %>%
#   select(商品标识,订单时间,商品总金额,会员编号)%>%mutate_at(vars(订单时间),as.Date)
# consum_data<-na.omit(consum_data)
#####################################################################################
#####################################################################################
#关联规则
Rules_transcation_data<-consum_data_2%>%select(会员编号,商品标识)
names(Rules_transcation_data)<-c("mem_id","shop_id")
print(dim(Rules_transcation_data))
Rules_transcation_data_1<-Rules_transcation_data%>%filter(shop_id%in%summary$vs)
print(dim(Rules_transcation_data_1))
Rules_transcation_data_test<-Rules_transcation_data_1%>%group_by(mem_id)%>%
  dplyr::summarise(count=n())%>%arrange(desc(count))%>%data.frame()
print(summary(Rules_transcation_data_test))

write.table(Rules_transcation_data_1,file = "E:/徐博士临时需求/guanlianshuru.txt",sep=",",quote = FALSE,fileEncoding = "UTF-8",row.names=FALSE)
#判断数据是否适合关联规则
if(median(Rules_transcation_data_test$count)<2){
 print("此数据不适合用关联规则") 
}
if(median(Rules_transcation_data_test$count)>=2){
  # Rules_transcation_data_1<-Rules_transcation_data%>%group_by(mem_id)%>%
  #   dplyr::summarise(shop_id=list(paste(shop_id,sep = "")))
  # Rules_transcation_data_1<-dcast(Rules_transcation_data,mem_id~...)
  xiaofeirules_duqu<-read.transactions("E:/徐博士临时需求/guanlianshuru.txt",format="single",cols=c(1,2),sep = ",",encoding = "UTF-8")
  print("读取完毕")
  print(summary(xiaofeirules_duqu))
  #barplot(sort(itemFrequency(xiaofeirules_duqu),decreasing = TRUE)[1:20])
  print("生成规则")
  #Rules_transcation_data_2<-as(Rules_transcation_data_1,"transactions")
  print("生成规则，开始数据规整")
  xiaofeirules_rules<-apriori(xiaofeirules_duqu,parameter=list(minlen=2,maxlen=2,supp=0.0004,conf=0.01,target="rules"))
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
  write.table(xiaofeirules_rules,file = "E:/徐博士临时需求/guanlianshuchu.txt",sep=",",quote = FALSE,fileEncoding = "UTF-8")
  print("写文件完毕，此次任务结束")
}
#关联潜客挖掘xiaofeirules_rules,Rules_transcation_data
#首先打通商品标识和商家名称
xiaofeirules_rules<-xiaofeirules_rules%>%data.frame()
# consum_data_3<-consum_data_2<-consum_data_2%>%select(商家名称,商品标识)%>%unique()
# names(consum_data_3)<-c("shop_name_front","front_shopid")
# k<-consum_data_3%>%group_by(front_shopid)%>%dplyr::summarise(count=n())%>%arrange(desc(count))
# 
# left_j_1<-left_join(xiaofeirules_rules,consum_data_3,by=c("front_shopid"))
print("找出所有后项")
hehind_shopid<-unique(xiaofeirules_rules$hehind_shopid)
print(paste("一共有",length(hehind_shopid),"个后项"),sep="")

rule_potential_mem_fun<-function(x){
  #元rules
  yuan_rule_table<-xiaofeirules_rules%>%filter(hehind_shopid%in%x)
  #print(head(yuan_rule_table))
  #元mem_shop
  yuan_mem_shop_table<-Rules_transcation_data
  names(yuan_mem_shop_table)[2]<-c("front_shopid")
  #print(head(yuan_mem_shop_table))
  #元rules与元mem_shop前项关联，打通初始潜客和后项
  yuan_join_table<-left_join(yuan_mem_shop_table,yuan_rule_table,by=c("front_shopid"))
  yuan_join_table<-na.omit(yuan_join_table)%>%arrange(desc(confidence))
  #print(dim(yuan_join_table))
  #初始潜客去掉熟客,真实潜客
  behind_mem_id<-yuan_mem_shop_table%>%filter(front_shopid%in%x)
  #print(dim(behind_mem_id))
  potentail_consumer<-setdiff(yuan_join_table$mem_id,behind_mem_id$mem_id)%>%unique()
  #print(length(potentail_consumer))
  #潜客指标携带
  potentail_consumer_1<-yuan_join_table%>%filter(mem_id%in%potentail_consumer)%>%
    select(hehind_shopid,mem_id,confidence,lift)%>%group_by(hehind_shopid,mem_id)%>%
    dplyr::summarise(con=max(confidence),lift=max(lift))%>%data.frame()%>%arrange(desc(con))
  #print(dim(potentail_consumer_1))
}
# k<-lapply(x,rule_potential_mem_fun)
# k1<-rbindlist(k)
rule_potential_mem_ally<-lapply(hehind_shopid,rule_potential_mem_fun)
rule_potential_mem_data<-rbindlist(rule_potential_mem_ally)%>%data.frame()
print(head(rule_potential_mem_data))
#打通会员编号和商铺名称
consum_data_3<-consum_data_1%>%select(商家名称,会员编号)%>%unique()
names(consum_data_3)<-c("shop_name","mem_id")
temp_1<-left_join(consum_data_3,rule_potential_mem_data,by=c("mem_id"))
temp_2<-temp_1%>%select(hehind_shopid,mem_id,shop_name,con,lift)
#打通商品标识和商铺名称
consum_data_4<-consum_data_1%>%select(商家名称,商品标识)%>%unique()
names(consum_data_4)<-c("shop_name_1","hehind_shopid")
temp_3<-left_join(temp_2,consum_data_4,by=c("hehind_shopid"))
temp_4<-temp_3%>%select(hehind_shopid,shop_name_1,mem_id,shop_name,con,lift)%>%arrange(desc(hehind_shopid))
temp_5<-na.omit(temp_4)
write.table(temp_5,file = "E:/徐博士临时需求/Rules_result.txt",sep=",",quote = FALSE,fileEncoding = "UTF-8")

# x1<-rule_potential_mem_data%>%filter(hehind_shopid%in%x)
# x1<-x1$mem_id
# x2<-Rules_transcation_data%>%filter(shop_id%in%x)
# x2<-x2$mem_id
# intersect(x1,x2)
#x<-hehind_shopid[3]
#####################################################################################
#####################################################################################
#基于消费金额的消费金额协同过滤
Recommender_data<-consum_data
names(Recommender_data)<-c("store_num","time","adj_amt","mem_num")
Recommender_data$refund_amt<-0
amount_input<-c("E:/徐博士临时需求/amount2.txt")
write.table(Recommender_data,file = "E:/徐博士临时需求/amount_input.txt",sep=",",quote = FALSE,fileEncoding = "UTF-8")

Recommender_data_sum<-Recommender_data%>%group_by(mem_num,store_num,time)%>%
  dplyr::summarise(amount=(sum(adj_amt,na.rm = TRUE)-sum(refund_amt,na.rm = TRUE)))%>%data.frame()%>%select(mem_num,store_num,amount)
Recommender_data_sum_dcast<-dcast(Recommender_data_sum,mem_num~...)%>%data.frame()
write.table(Recommender_data_sum_dcast,file = "E:/徐博士临时需求/amount.txt",sep=",",quote = FALSE,fileEncoding = "UTF-8")
out_file<-c("E:/徐博士临时需求/amount2.txt")
R_F_S_Model<-function(in_file,out_file){
  #xiaofei_input_data<-fread(in_file,sep = ",",encoding = "UTF-8")
  xiaofei_input_data<-in_file
  xiaofei_input_data_1<-subset(xiaofei_input_data,adj_amt!=0|refund_amt!=0)
  for(i in 1:ncol(xiaofei_input_data_1)){
    xiaofei_input_data_1[,i]<-plyr::mapvalues(xiaofei_input_data_1[,i],0,
                                              NA)
  }
  xiaofei_input_data_1<-xiaofei_input_data_1%>%arrange(store_num)
  xiaofei_input_data_2<-xiaofei_input_data_1%>%group_by(mem_num,store_num)%>%
    dplyr::summarise(amount=(sum(adj_amt,na.rm = TRUE)-sum(refund_amt,na.rm = TRUE)))%>%
    data.frame()
  xiaofei_input_data_3<-dcast(xiaofei_input_data_2,mem_num~...)%>%data.frame()
  names(xiaofei_input_data_3)[1]<-c("id")
  xiaofei_input_data_4<-xiaofei_input_data_3%>%select(c(2:dim(xiaofei_input_data_3)[2],1))
  x<-dim(xiaofei_input_data_1)[1]
  y<-dim(xiaofei_input_data_3)[2]
  y1<-round(y/2)
  x_split<-xiaofei_input_data_1[round(x/2),]
  x_split_point_1<-which(names(xiaofei_input_data_4)==paste("X",x_split$store_num,sep = ""))
  x_split_point_2<-round(abs(y1-x_split_point_1)/2)
  x_split_point_3<-x_split_point_1-x_split_point_2
  x_split_point_4<-x_split_point_3+1
  df<-xiaofei_input_data_4
  print(x_split_point_4)
  print(y)
  for(i in x_split_point_4:(y-1)){
    #购买人数
    print("开始执行")
    print(paste("店铺分隔点为",x_split_point_3,sep=""))
    print(names(df)[i])
    model_data_1<-subset(df,df[,i]>0)
    print(dim(model_data_1))
    model_data<-model_data_1%>%select(-id)
    model_data<-model_data%>%as.matrix()
    model_data<-as(model_data,"realRatingMatrix")
    df_1<-df
    df_1[,i][is.na(df_1[,i])]<--1
    predict_data_1<-subset(df_1,df_1[,i]==-1)
    predict_data_1[,i]<-NA
    predict_data<-predict_data_1%>%select(-id)
    print(dim(predict_data))
    predict_data<-predict_data%>%as.matrix()
    predict_data<-as(predict_data,"realRatingMatrix")
    print("转换完毕")
    ###################################################################
    #IBCF PCA POPULAR RANDOM SVD 
    ###################################################################
    xietong_model<-Recommender(model_data,method="IBCF")
    print(xietong_model)
    ml_predict<-predict(xietong_model,predict_data,type="ratings")
    ml_matrix<-as(ml_predict,"matrix")
    ml_data.frame_1<-ml_matrix%>%data.frame()
    ml_data.frame_1<-ml_data.frame_1%>%mutate(hybh=predict_data_1$id)
    ml_data.frame<-subset(ml_data.frame_1,ml_data.frame_1[,i]>0)
    ml_data.frame<-ml_data.frame%>%arrange(desc(ml_data.frame[,i]))
    output_data<-cbind(ml_data.frame$hybh,ml_data.frame[,i])%>%data.frame()
    print(head(output_data))
    print(dim(output_data))
    temp<-paste(names(ml_data.frame_1)[i],output_data[,1],sep = "$")
    temp_1<-paste(temp,output_data[,2],sep = "$")
    temp_2<-paste(temp_1,dim(model_data_1)[1],sep = "$")
    print(head(temp_1))
    print("开始写文件")
    write.table(temp_2,file = out_file,row.names = FALSE,col.names=FALSE,sep = ",",fileEncoding = "UTF-8",quote = FALSE,append = TRUE)
    print(i)
    print("完毕")
  }
  print("所有任务已执行完毕")
  
}

amount2<-R_F_S_Model(Recommender_data,out_file)


















