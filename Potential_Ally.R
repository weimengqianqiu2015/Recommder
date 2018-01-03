library(dplyr)
library(data.table)
library(recommenderlab)
library(Tmisc)
library(plyr)
library(tidyr)
library(proxy)
library(stringr)
Args <- commandArgs()
#####数据输入输出路径
re_amount1_file=Args[6]
re_amount2_file=Args[7]
re_count1_file=Args[8]
re_count2_file=Args[9]
guanlian_file=Args[10]
out_put_file=Args[11]
Potential_Ally<-function(re_amount1_file,re_amount2_file,re_count1_file,re_count2_file,guanlian_file,out_put_file){
  ###################################################################
  #读取并处理amount协同过滤数据
  print("读取并处理amount协同过滤数据")
  result_amount1<-fread(re_amount1_file,header = FALSE)%>%data.frame()
  result_amount2<-fread(re_amount2_file,header = FALSE)%>%data.frame()
  result_amount1<-na.omit(result_amount1)
  result_amount2<-na.omit(result_amount2)
  print (paste("result_amount1有",dim(result_amount1)[1],"行",sep = ""))
  print (paste("result_amount2有",dim(result_amount2)[1],"行",sep = ""))
  result_amount_1<-rbind(result_amount1,result_amount2)
  print (paste("result_amount1与result_amount2合并后有",dim(result_amount_1)[1],"行",sep = ""))
  result_amount<-unique(result_amount_1)%>%data.frame()
  print (paste("result_amount1与result_amount2合并后去重有",dim(result_amount)[1],"行",sep = ""))
  if(dim(result_amount_1)[1]-dim(result_amount)[1]>0)print("协同过滤amount数据有重复现象")
  result_amount_1<-separate(result_amount,V1,c("shop_id","mem_num","re_amount_index","xiaofei_count"),sep="\\$")
  result_amount_2<-result_amount_1%>%select(c(1:3))
  result_amount_3<-result_amount_2%>%mutate_at(vars(re_amount_index),as.numeric)%>%arrange(re_amount_index)
  result_amount_4<-unite(result_amount_3,shop_id_mem_num,c(shop_id,mem_num),sep = "$")
  ####################################################################
  #读取并处理count协同过滤数据
  print("读取并处理count协同过滤数据")
  result_count1<-fread(re_count1_file,header = FALSE)%>%data.frame()
  result_count2<-fread(re_count2_file,header = FALSE)%>%data.frame()
  result_count1<-na.omit(result_count1)
  result_count2<-na.omit(result_count2)
  print (paste("result_count1有",dim(result_count1)[1],"行",sep = ""))
  print (paste("result_count2有",dim(result_count2)[1],"行",sep = ""))
  result_count_1<-rbind(result_count1,result_count2)
  print (paste("result_count1与result_count2合并后有",dim(result_count_1)[1],"行",sep = ""))
  result_count<-unique(result_count_1)%>%data.frame()
  print (paste("result_count1与result_count2合并后去重有",dim(result_count)[1],"行",sep = ""))
  if(dim(result_count_1)[1]-dim(result_count)[1]>0)print("协同过滤count数据有重复现象")
  result_count_1<-separate(result_count,V1,c("shop_id","mem_num","re_count_index","xiaofei_count"),sep="\\$")
  result_count_2<-result_count_1%>%select(c(1:3))
  result_count_3<-result_count_2%>%mutate_at(vars(re_count_index),as.numeric)%>%arrange(re_count_index)
  result_count_4<-unite(result_count_3,shop_id_mem_num,c(shop_id,mem_num),sep = "$")
  #####################################################################
  #采用半连接合并两种协同过滤结果
  print("采用半连接合并两种协同过滤结果")
  amount_count_inner<-plyr::join(result_amount_4,result_count_4,by = intersect(names(result_amount_4)[1], names(result_count_4)[1]),
                                 type = "full", match = "first")
  amount_count_inner<-na.omit(amount_count_inner)
  print(head(amount_count_inner))
  print(tail(amount_count_inner))
  print("协同合并后一个有",dim(amount_count_inner)[1],"行",sep="")
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
                                    type="full",match="first")
  amount_count_guanlian<-na.omit(amount_count_guanlian)
  print(head(amount_count_guanlian))
  print(tail(amount_count_guanlian))
  print("关联与协同合并后一个有",dim(amount_count_guanlian)[1],"行",sep="")
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

