#第一题code
#读取数据
weight <- read.csv("q1weight.csv") # 和危害度相关的特征的权重
q1data <- read.csv("q1data.csv") # 与危害度相关的特征的GTD数据
data <- q1data[c(1:114183),]

#计算得分
score <- data.frame("eventid"=data$eventid, stringsAsFactors=F)
for(i in 1:13){
  score<- cbind(score, data[,i+1]*weight[i,2])
}
riskscore <- apply(score[,2:14],1,sum)
result <- data.frame("eventid"=data$eventid,riskscore, stringsAsFactors=F)
write.csv(result,file = "ans1.csv") # 输出危害度得分
summary(riskscore)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   9.000   9.513   9.138  10.030  52.660 

#生成级别
class <- rep(1:5,c(15, 1004, 19079,29129,64956))
result_order <- result[order(result$riskscore, decreasing=T),]
q1_class <- data.frame("eventid"= result_order$eventid, class, stringsAsFactors=F)
write.csv(q1_class, 'q1_classification.csv') # 输出级别
