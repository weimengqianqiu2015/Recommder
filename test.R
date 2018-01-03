library(dplyr)
library(data.table)
library(recommenderlab)
library(Tmisc)
library(plyr)
library(tidyr)
library(proxy)
library(stringr)
test_amount_4<-fread("E:/test_4/test_amout_4.txt",sep=",")%>%data.frame()
test_count_4<-fread("E:/test_4/test_count_4.txt",sep=",")%>%data.frame()
test_amount_4_1<-test_amount_4%>%group_by(shop_id_mem_num,re_amount_index)%>%
  dplyr::summarise(count=n())%>%select(shop_id_mem_num,re_amount_index)
  
a<-function(re_amount1_file,re_amount2_file){
  amount_count_inner<-plyr::join(re_amount1_file,re_amount2_file,by = intersect(names(result_amount_4)[1], names(result_count_4)[1]),
                                 type = "inner", match = "first")
  return(amount_count_inner)
}
