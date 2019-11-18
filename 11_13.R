# 과제 11.13
de<- read.csv("http://webecono.da.to/rdata/default.csv")
str(de)
head(de)

# 데이터를 50%로 분할. 
set.seed(10)
n=nrow(de);n
train<-sample(n,floor(n/2))

### logistic
# 1) logistic 회귀모형을 추정.
log.fit <- glm(default~., data=de[train,], family=binomial)
# 2) 추정결과변수를 사용하여 예측확률 구한다.
log.prob <-predict(log.fit, newdata=de[-train,], type="response")
# 3) 50%기준으로 예측확률을 분류한다.
log.pred <- rep("No", length(log.prob))
# 4) 예측확률이 50%넘으면 No를 Yes로 대체한다.
log.pred[log.prob>0.5]="Yes"
# 정확도.
p1<-mean(log.pred == de$default[-train]);p1

### rpart
library(rpart)
de_rpart<- rpart(default~., data=de[train,])
library("rpart.plot")
rpart.plot(de_rpart)
pred <- predict(de_rpart, newdata=de[-train,], type="class")
# confusion matrix
addmargins(table(pred, de$default[-train]))
#예측정확도
p2<-mean(pred==de$default[-train]);p2

### ctree
library(party)
de_fit <- ctree(default~., data=de[train,])
plot(de_fit)
pred <- predict(de_fit, newdata=de[-train,], type="response")
#예측정확도
p3<-mean(pred==de$default[-train]);p3

### randomforest
m= ncol(de)/3
library(randomForest)
rf.fit <- randomForest(default~., data=de[train,], mtry=m, importance=TRUE)
pred <- predict(rf.fit, newdata=de[-train,], type="response")

###
# mtry는 범주형 일때, 변수 갯수만큼 반복가능한거.
forest_p<-rep(0,3)
for(i in 1:3){
  rf.fit <- randomForest(default~., data=de[train,], mtry=i, importance=TRUE)
  pred <- predict(rf.fit, newdata=de[-train,], type="response")
  forest_p[i]<-mean(pred==de$default[-train])
  print(paste(i,":",forest_p[i]))
}
cat(match(max(forest_p), forest_p),":",max(forest_p))
p4<-max(forest_p);p4
###
#예측정확도
p4<-mean(pred==de$default[-train]);p4

p1;p2;p3;p4
