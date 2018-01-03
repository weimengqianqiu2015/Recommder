library(dplyr)
library(data.table)
library(recommenderlab)
library(Tmisc)
library(plyr)
library(tidyr)
library(proxy)
library(stringr)
# Args <- commandArgs()
# #####数据输入输出路径
# re_amount1_file=Args[6]
# re_amount2_file=Args[7]
# re_count1_file=Args[8]
# re_count2_file=Args[9]
# guanlian_file=Args[10]
# out_put_file=Args[11]
result_count1<-fread("E:/huarun_r/PoAlly_test/result_count1.txt")
Potential_Ally<-function(re_amount1_file,re_amount2_file,re_count1_file,re_count2_file,guanlian_file,out_put_file){
  ###################################################################
  #读取并处理amount协同过滤数据
  print("读取并处理amount协同过滤数据")
  result_amount1<-fread(re_amount1_file,header = FALSE)
  result_amount2<-fread(re_amount2_file,header = FALSE)
  result_amount1<-result_amount1%>%data.frame()
  result_amount2<-result_amount2%>%data.frame()
  result_amount1<-na.omit(result_amount1)
  result_amount2<-na.omit(result_amount2)
  result_amount<-rbind(result_amount1,result_amount2)
  result_amount_1<-separate(result_amount,V1,c("shop_id","mem_num","re_amount_index","xiaofei_count"),sep="\\$")
  result_amount_2<-result_amount_1%>%select(c(1:3))
  result_amount_3<-result_amount_2%>%mutate_at(vars(re_amount_index),as.numeric)%>%arrange(re_amount_index)
  result_amount_4<-unite(result_amount_3,shop_id_mem_num,c(shop_id,mem_num),sep = "$")
  ####################################################################
  #读取并处理count协同过滤数据
  print("读取并处理count协同过滤数据")
  result_count1<-fread(re_count1_file,header = FALSE)
  result_count2<-fread(re_count2_file,header = FALSE)
  result_count1<-result_count1%>%data.frame()
  result_count2<-result_count2%>%data.frame()
  result_count1<-na.omit(result_count1)
  result_count2<-na.omit(result_count2)
  result_count<-rbind(result_count1,result_count2)
  result_count_1<-separate(result_count,V1,c("shop_id","mem_num","re_count_index","xiaofei_count"),sep="\\$")
  result_count_2<-result_count_1%>%select(c(1:3))
  result_count_3<-result_count_2%>%mutate_at(vars(re_count_index),as.numeric)%>%arrange(re_count_index)
  result_count_4<-unite(result_count_3,shop_id_mem_num,c(shop_id,mem_num),sep = "$")
  #####################################################################
  #采用半连接合并两种协同过滤结果
  print("采用半连接合并两种协同过滤结果")
  amount_count_inner<-plyr::join(result_amount_4,result_count_4,by = intersect(names(result_amount_4)[1], names(result_count_4)[1]),
                                 type = "inner", match = "first")
  amount_count_inner_1<-separate(amount_count_inner,shop_id_mem_num,c("shop_id","mem_num"),sep = "\\$")
  #####################################################################
  #读取并处理关联潜客结果
  print("读取并处理关联潜客结果")
  guanlian<-fread(guanlian_file,header = FALSE,sep = ",")
  guanlian<-guanlian%>%data.frame()
  guanlian<-unite(guanlian,"shop_id_mem_num",c(V1,V2),sep = "$")
  #####################################################################
  #合并关联结果和协同过滤结果
  print("合并关联结果和协同过滤结果")
  amount_count_guanlian<-plyr::join(amount_count_inner,guanlian,by= intersect(names(amount_count_inner)[1],
                                                                              names(guanlian)[1]),
                                    type="inner",match="first")
  amount_count_guanlian_1<-separate(amount_count_guanlian,shop_id_mem_num,c("shop_id","mem_num"),sep = "\\$")
  ####################################################################
  #将结果格式改为前端需要格式
  print("将结果格式改为前端需要格式")
  amount_count_guanlian_result_1<-amount_count_guanlian_1%>%group_by(shop_id)%>%arrange(desc(V4),desc(re_count_index),
                                                                                        desc(re_amount_index))%>%dplyr::mutate(range=seq(1:n()))%>%arrange(shop_id)%>%data.frame()
  amount_count_guanlian_result_2<-amount_count_guanlian_result_1%>%select(shop_id,mem_num,range)
  
  inner_name<-unique(amount_count_guanlian_1$shop_id)
  amount_count_inner_2<-amount_count_inner_1%>%filter(!shop_id%in%inner_name)
  amount_count_inner_3<-amount_count_inner_2%>%group_by(shop_id)%>%arrange(desc(re_count_index),desc(re_amount_index))%>%dplyr::mutate(range=seq(1:n()))
  amount_count_inner_4<-amount_count_inner_3%>%select(shop_id,mem_num,range)%>%data.frame()
  qianke_result<-rbind(amount_count_inner_4,amount_count_guanlian_result_2)
  qianke_result<-na.omit(qianke_result)
  write.table(qianke_result,file = out_put_file,sep=",",col.names=TRUE,quote = FALSE,row.names = FALSE,fileEncoding = "UTF-8")
  print("任务执行完毕")
}

Potential_Ally(re_amount1_file,re_amount2_file,re_count1_file,re_count2_file,guanlian_file,out_put_file)

