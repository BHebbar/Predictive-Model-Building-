assign2 <- read.csv(file.choose(),header=T)
attach(assign2)

#logistic regression
glm.fit1 <- glm(Death~.,data=assign2,family = binomial)
summary(glm.fit1)
coef(glm.fit1)  

#knn
  for(i in 1:ncol(assign2)){
    assign2[is.na(assign2[,i]), i] <- mean(assign2[,i], na.rm = TRUE)}

library(class)
trainset <- assign2[1:216,]
testset<- assign2[1:216,]
knn.p0= knn(trainset[,-22],testset[,-22], trainset$Death,k=1)
verror.knn = 1-mean(testset$Death==knn.p0)
verror.knn

#CV
library(boot)
k=10
set.seed(33)
test.lr = rep(0, times=k)
test.knn = rep(0,times=k)
folds=sample(1:k, nrow(assign2),replace=TRUE)
for(j in 1:10){
  glm.fit = glm(formula= Death~.,family = binomial, data=assign2[folds != j,])
  predict.p=predict(glm.fit,assign2[folds==j,],type = "response")
  pred= predict.p>0.5
  
  test.lr[j] = 1-mean(assign2$Death[folds==j]==pre)
  m=table(assign2[folds==j,22],pred)
  
  library(class)
  knn.p= knn(assign2[folds != j,-22],assign2[folds==j,-22], assign2$Death[folds !=j],k=1)
  test.knn[j] = 1-mean(assign2$Death[folds==j]==knn.p)
}

test.lr
test.knn


#validation set approach 1
set.seed(33)
train=sample(216,118)
glm.fit2 <- glm(Death~.,data=assign2,subset=train)

#validation set error 1
mean((Death-predict(glm.fit2,assign2))[-train]^2,na.rm = TRUE)

#validation set approach 2
LR2
set.seed(33)
train1=sample(216,80)
glm.fit3 <- glm(Death~.,data=assign2,subset=train1)

#validation set error 2
mean((Death-predict(glm.fit3,assign2))[-train1]^2,na.rm = TRUE)

#validation set approach 3
set.seed(33)
train2=sample(216,165)
glm.fit4 <- glm(Death~.,data=assign2,subset=train2)

#validation set error 3
mean((Death-predict(glm.fit4,assign2))[-train2]^2,na.rm = TRUE)

#KNN1 
trainset1 <- assign2[1:130,]
testset1<- assign2[131:216,]
knn.p1= knn(trainset1[,-22],testset1[,-22], trainset1$Death ,k=1)
verror1.knn = 1-mean(testset1$Death==knn.p1)
verror1.knn

#KNN2
trainset2 <- assign2[1:170,]
testset2<- assign2[171:216,]
knn.p2= knn(trainset2[,-22],testset2[,-22], trainset2$Death ,k=1)
verror2.knn = 1-mean(testset2$Death==knn.p2)
verror2.knn

#KNN3
trainset3 <- assign2[1:81,]
testset3<- assign2[82:216,]
knn.p3= knn(trainset3[,-22],testset3[,-22], trainset3$Death ,k=1)
verror3.knn = 1-mean(testset3$Death==knn.p3)
verror3.knn












