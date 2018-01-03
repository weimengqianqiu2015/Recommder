library(dplyr)
library(data.table)
library(recommenderlab)
library(Tmisc)
library(plyr)
library(tidyr)
library(proxy)
library(methods)
library(stringr)
Args <- commandArgs()
#####数据输入路径
in_file=Args[6]
in_file<-c(in_file)
#####数据输出路径
out_file=Args[7]
out_path<-c(out_file)
###############################################################
#协同过滤模型
R_C_S_Model<-function(in_file,out_path){
  xiaofei_input_data<-fread(in_file,sep = ",",encoding = "UTF-8")
  xiaofei_input_data<-xiaofei_input_data%>%data.frame()
  xiaofei_input_data_1<-subset(xiaofei_input_data,adj_amt!=0|refund_amt!=0)
  for(i in 1:ncol(xiaofei_input_data_1)){
    xiaofei_input_data_1[,i]<-plyr::mapvalues(xiaofei_input_data_1[,i],0,
                                              NA)
  }
  xiaofei_input_data_1<-xiaofei_input_data_1%>%arrange(store_num)
  xiaofei_input_data_2<-xiaofei_input_data_1%>%group_by(mem_num,store_num)%>%dplyr::summarise(amount=(sum(adj_amt,na.rm = TRUE)-sum(refund_amt,na.rm = TRUE)))
  xiaofei_input_data_3<-dcast(xiaofei_input_data_2,mem_num~...)%>%data.frame()
  names(xiaofei_input_data_3)[1]<-c("id")
  xiaofei_input_data_4<-xiaofei_input_data_3%>%select(c(2:dim(xiaofei_input_data_3)[2],1))
  x<-dim(xiaofei_input_data_1)[1]
  y<-dim(xiaofei_input_data_3)[2]
  y1<-round(y/2)
  x_split<-xiaofei_input_data_1[round(x/2),]
  x_split_point_1<-which(names(xiaofei_input_data_4)==x_split$store_num)
  x_split_point_2<-round(abs(y1-x_split_point_1)/2)
  x_split_point_3<-x_split_point_1-x_split_point_2
  x_split_point_4<-x_split_point_3+1
  #re_amount_1_data<-xiaofei_input_data_3[,1:x_split_point_1]
  #re_amount_1_data$id<-xiaofei_input_data_3$id
  #re_amount_2_data<-xiaofei_20150808_20160808_3[,x_split_point_2:y]
  df<-xiaofei_input_data_4
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
    #print("模型完毕")
    print(xietong_model)
    ml_predict<-predict(xietong_model,predict_data,type="ratings")
    ml_matrix<-as(ml_predict,"matrix")
    ml_data.frame_1<-ml_matrix%>%data.frame()
    ml_data.frame_1<-ml_data.frame_1%>%mutate(hybh=predict_data_1$id)
    ml_data.frame<-subset(ml_data.frame_1,ml_data.frame_1[,i]>0)
    ml_data.frame<-ml_data.frame%>%arrange(desc(ml_data.frame[,i]))
    output_data<-cbind(ml_data.frame$hybh,ml_data.frame[,i])%>%data.frame()
    #write.csv(output_data[,1],file = "E:/hybh.csv")
    print(head(output_data))
    print(dim(output_data))
    temp<-paste(names(ml_data.frame_1)[i],output_data[,1],sep = "$")
    temp_1<-paste(temp,output_data[,2],sep = "$")
    temp_2<-paste(temp_1,dim(model_data_1)[1],sep = "$")
    print(head(temp_1))
    print("开始写文件")
    write.table(temp_2,file = out_path,row.names = FALSE,col.names=FALSE,sep = ",",fileEncoding = "UTF-8",quote = FALSE,append = TRUE)
    print(i)
    print("完毕")
  }
  print("所有任务已执行完毕")
  
}

R_C_S_Model(in_file,out_path)




