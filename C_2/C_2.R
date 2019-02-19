C_2=read.csv("C-2-V1_input.csv",header = T,sep=",")
row.names(C_2)=C_2[,1]
C_2=C_2[,-7]
data=as.data.frame(C_2[,c(2:6)])
set.seed(1)
install.packages("klaR")
library("klaR")

#确定聚类的个数
install.packages(mclust)
library(mclust)
m_clust<- Mclust(as.matrix(data[1:1000,]), G=1:20)
summary(m_clust)
plot(m_clust, "BIC")


## run algorithm on x:
cl <- kmodes(data, 15)
summary(cl )
#cluster的可视化
## and visualize with some jitter:
data=as.data.frame(data)
plot(data[1:5000,], col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)
data$cluster<-cl[["cluster"]]
data=as.data.frame(data)


#将cluster的结果写到本地文件中
write(data,"cluster_result.csv")

#导入风险分级数据
riskscore<-read.csv("ans1.csv",header = TRUE,sep=",")
row.names(riskscore)=riskscore[,1]

#分类数据与风险分级数据合并
cluster_score=merge(data,riskscore,by="row.names")
cluster_score=cluster_score[,-1]
#将文件写入本地
write.csv(cluster_score,"cluster_score.csv")

#确定每一个类的平均riskscore
##利用正则表达式提取每个分类的数据(此处提取出来的数据为行号)
cluster1=grep("^1$",cluster_score$cluster)
cluster2=grep("^2$",cluster_score$cluster)
cluster3=grep("^3$",cluster_score$cluster)
cluster4=grep("^4$",cluster_score$cluster)
cluster5=grep("^5$",cluster_score$cluster)
cluster6=grep("^6$",cluster_score$cluster)
cluster7=grep("^7$",cluster_score$cluster)
cluster8=grep("^8$",cluster_score$cluster)
cluster9=grep("^9$",cluster_score$cluster)
cluster10=grep("^10$",cluster_score$cluster)
cluster11=grep("^11$",cluster_score$cluster)
cluster12=grep("^12$",cluster_score$cluster)
cluster13=grep("^13$",cluster_score$cluster)
cluster14=grep("^14$",cluster_score$cluster)
cluster15=grep("^15$",cluster_score$cluster)
table(cluster_score$cluster)

#求每个cluster的riskscore的平均值
cluster1=cluster_score[cluster1,]
cluster1_score=mean(cluster1$riskscore)
cluster2=cluster_score[cluster2,]
cluster2_score=mean(cluster2$riskscore)
cluster3=cluster_score[cluster3,]
cluster3_score=mean(cluster3$riskscore)
cluster4=cluster_score[cluster4,]
cluster4_score=mean(cluster4$riskscore)
cluster5=cluster_score[cluster5,]
cluster5_score=mean(cluster5$riskscore)
cluster6=cluster_score[cluster6,]
cluster6_score=mean(cluster6$riskscore)
cluster7=cluster_score[cluster7,]
cluster7_score=mean(cluster7$riskscore)
cluster8=cluster_score[cluster8,]
cluster8_score=mean(cluster8$riskscore)
cluster9=cluster_score[cluster9,]
cluster9_score=mean(cluster9$riskscore)
cluster10=cluster_score[cluster10,]
cluster10_score=mean(cluster10$riskscore)
cluster11=cluster_score[cluster11,]
cluster11_score=mean(cluster11$riskscore)
cluster12=cluster_score[cluster12,]
cluster12_score=mean(cluster12$riskscore)
cluster13=cluster_score[cluster13,]
cluster13_score=mean(cluster13$riskscore)
cluster14=cluster_score[cluster14,]
cluster14_score=mean(cluster14$riskscore)
cluster15=cluster_score[cluster15,]
cluster15_score=mean(cluster15$riskscore)

#将每个类的mean_risk_score写入到每个cluster中
cluster1$mean_riskscore<-rep(mean(cluster1$riskscore),times=nrow(cluster1))
cluster2$mean_riskscore<-rep(mean(cluster2$riskscore),times=nrow(cluster2))
cluster3$mean_riskscore<-rep(mean(cluster3$riskscore),times=nrow(cluster3))
cluster4$mean_riskscore<-rep(mean(cluster4$riskscore),times=nrow(cluster4))
cluster5$mean_riskscore<-rep(mean(cluster5$riskscore),times=nrow(cluster5))
cluster6$mean_riskscore<-rep(mean(cluster6$riskscore),times=nrow(cluster6))
cluster7$mean_riskscore<-rep(mean(cluster7$riskscore),times=nrow(cluster7))
cluster8$mean_riskscore<-rep(mean(cluster8$riskscore),times=nrow(cluster8))
cluster9$mean_riskscore<-rep(mean(cluster9$riskscore),times=nrow(cluster9))
cluster10$mean_riskscore<-rep(mean(cluster10$riskscore),times=nrow(cluster10))
cluster11$mean_riskscore<-rep(mean(cluster11$riskscore),times=nrow(cluster11))
cluster12$mean_riskscore<-rep(mean(cluster12$riskscore),times=nrow(cluster12))
cluster13$mean_riskscore<-rep(mean(cluster13$riskscore),times=nrow(cluster13))
cluster14$mean_riskscore<-rep(mean(cluster14$riskscore),times=nrow(cluster14))
cluster15$mean_riskscore<-rep(mean(cluster15$riskscore),times=nrow(cluster15))

#计算前五类并输出结果
cluster_mean <- matrix(NA,nrow=15,ncol=2)
cluster_mean[,1]=rep(1:15)
cluster_mean[,2]=c(cluster1_score,cluster2_score,cluster3_score,cluster4_score,cluster5_score,cluster6_score,
                   cluster7_score,cluster8_score,cluster9_score,cluster10_score,cluster11_score,cluster12_score,
                   cluster13_score,cluster14_score,cluster15_score)
write.csv(cluster_mean,"cluster_mean.csv")

#将前五类的结果输出
cluster_top5=rbind(cluster9,cluster10,cluster4,cluster5,cluster12)
write.csv(cluster_top5,"cluster_top5.csv")

#找出前五类的聚类中心-即每一个类的modes
cluster_modes=cl[["modes"]]
cluster_modes$meanscore=cluster_mean[,2]
write.csv(cluster_modes,"cluster_modes.csv")
#找出前五类的modes
cluster_modes_top5=cluster_modes[c(9,10,4,5,12),]
cluster_modes_top5=cluster_modes_top5[,-6]

#install.packages(hamming.distance)
install.packages(e1071)
library(e1071)
#计算所有数据样本与第9类的 Hamming distance
data=data[,-6]

#求每个事件与类之间的
cluster_modes_top5=as.matrix(cluster_modes_top5)
data=as.matrix(data)
cluster1_dist=matrix(NA,nrow(data),1)
for (i in 1:nrow(data)){
  cluster1_dist[i]=hamming.distance(data[i,],cluster_modes_top5[1,])
}


cluster2_dist=matrix(NA,nrow(data),1)
for (i in 1:nrow(data)){
  cluster2_dist[i]=hamming.distance(data[i,],cluster_modes_top5[2,])
}

cluster3_dist=matrix(NA,nrow(data),1)
for (i in 1:nrow(data)){
  cluster3_dist[i]=hamming.distance(data[i,],cluster_modes_top5[3,])
}

cluster4_dist=matrix(NA,nrow(data),1)
for (i in 1:nrow(data)){
  cluster4_dist[i]=hamming.distance(data[i,],cluster_modes_top5[4,])
}

cluster5_dist=matrix(NA,nrow(data),1)
for (i in 1:nrow(data)){
  cluster5_dist[i]=hamming.distance(data[i,],cluster_modes_top5[5,])
}

data=as.data.frame(data)
data_dist=cbind(data,cluster1_dist,cluster2_dist,cluster3_dist,cluster4_dist,cluster5_dist)

#write down the final result
data_dist=data_dist[,6:10]
write.csv(data_dist,"cluster_dist.csv")

#将距离转换为评分
L0=grep("^0$",data_dist$cluster1_dist)
data_dist$cluster1_dist[L0]<-1
L0=grep("^1$",data_dist$cluster1_dist)
data_dist$cluster1_dist[L0]<-2
L0=grep("^2$",data_dist$cluster1_dist)
data_dist$cluster1_dist[L0]<-3
L0=grep("^3$",data_dist$cluster1_dist)
data_dist$cluster1_dist[L0]<-4
L0=grep("^4$",data_dist$cluster1_dist)
data_dist$cluster1_dist[L0]<-5

#将评分进行排序
dist_order=data_dist
dist_order=as.data.frame(dist_order)

x<-NA
for (i in 1:nrow(dist_order)){
  x[i]=min(dist_order[i,])-1
  for(j in 1:5){
    dist_order[i,j]=dist_order[i,j]-x[i]
  }
}

#将排序结果写出来
write.csv(dist_order,"dist_order.csv")


