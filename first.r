assign1 <- read.csv(file.choose(),header=T)
attach(assign1)
model1 <- lm(HtVol~Male+Age+Ht+Wt)
summary(model1)
coefficients(model1)


library(quantreg)

model2 <- rq(HtVol~Male+Age+Ht+Wt)
summary(model2)
coefficients(model2)
# second

model3 <- lm(HtVol~Male+Age+BMI+BSA)
summary(model3)
coefficients(model3)

model4 <- rq(HtVol~Male+Age+BMI+BSA)
summary(model4)
coefficients(model4)

#cv
library(boot)
k=10
set.seed(1)
rmse.ls1=rep(0,times=k)
mae.ls1=rep(0,times=k)
rmse.lad1=rep(0,times=k)
mae.lad1=rep(0,times=k)
rmse.ls2=rep(0,times=k)
mae.ls2=rep(0,times=k)
rmse.lad2=rep(0,times=k)
mae.lad2=rep(0,times=k)
smdape.ls1=rep(0,times=k)
smdape.lad1=rep(0,times=k)
smdape.ls2=rep(0,times=k)
smdape.lad2=rep(0,times=k)
pred.ls1=rep(0,times=k)
pred.ls2=rep(0,times=k)
pred.lad1=rep(0,times=k)
pred.lad2=rep(0,times=k)
predict.ls1=rep(0,times=58)
predict.lad1=rep(0,times=58)
predict.ls2=rep(0,times=58)
predict.lad2=rep(0,times=58)
folds=sample(1:k,nrow(assign1),replace=TRUE)
for(j in 1:10){
# least square 
ls.fit1 =lm(HtVol~Male+Age+Ht+Wt,data =assign1[folds !=j,])
pred.ls1=predict(ls.fit1,assign1[folds==j,])

ls.fit2 =lm(HtVol~Male+Age+BMI+BSA,data =assign1[folds !=j,])
pred.ls2=predict(ls.fit2,assign1[folds==j,])

#root mean square error and mean absolute error for each fold
rmse.ls1[j]=sqrt(mean((assign1$HtVol[folds==j]-pred.ls1)^2))
mae.ls1[j]=mean(abs(assign1$HtVol[folds==j]-pred.ls1))

rmse.ls2[j]=sqrt(mean((assign1$HtVol[folds==j]-pred.ls2)^2))
mae.ls2[j]=mean(abs(assign1$HtVol[folds==j]-pred.ls2))

#predicted result for each observation
predict.ls1[folds==j]=pred.ls1
predict.ls2[folds==j]=pred.ls2

#least absolute deviations
lad.fit1=quantreg:: rq(HtVol~Male+Age+Ht+Wt,data =assign1[folds !=j,])
pred.lad1=predict(lad.fit1,assign1[folds==j,])

lad.fit2=quantreg:: rq(HtVol~Male+Age+BMI+BSA,data =assign1[folds !=j,])
pred.lad2=predict(lad.fit2,assign1[folds==j,])

#root mean square error and mean absolute error for each fold
rmse.lad1[j]=sqrt(mean((assign1$HtVol[folds==j]-pred.lad1)^2))
mae.lad1[j]=mean(abs(assign1$HtVol[folds==j]-pred.lad1))

rmse.lad2[j]=sqrt(mean((assign1$HtVol[folds==j]-pred.lad2)^2))
mae.lad2[j]=mean(abs(assign1$HtVol[folds==j]-pred.lad2))

#predicted result for each observation
predict.lad1[folds==j]=pred.lad1
predict.lad2[folds==j]=pred.lad2

#smdape for 4 models
smdape.ls1[j]=median(200*abs(assign1$HtVol[folds==j]-pred.ls1)/(assign1$HtVol[folds==j]+pred.ls1))
smdape.ls2[j]=median(200*abs(assign1$HtVol[folds==j]-pred.ls2)/(assign1$HtVol[folds==j]+pred.ls2))
smdape.lad1[j]=median(200*abs(assign1$HtVol[folds==j]-pred.lad1)/(assign1$HtVol[folds==j]+pred.lad1))
smdape.lad2[j]=median(200*abs(assign1$HtVol[folds==j]-pred.lad2)/(assign1$HtVol[folds==j]+pred.lad2))
}

#matrices
matrix1 = cbind(rmse.ls1,rmse.lad1,rmse.ls2,rmse.lad2)
matrix2 = cbind(mae.ls1,mae.lad1,mae.ls2,mae.lad2)
matrix3 = cbind(smdape.ls1,smdape.lad1,smdape.ls2,smdape.lad2)

rownames(matrix1) <- paste('Fold', 1:10)
rownames(matrix2) <- paste('Fold', 1:10)
rownames(matrix3) <- paste('Fold', 1:10)

colnames(matrix1) <- paste('Model', 1:4)
colnames(matrix2) <- paste('Model', 1:4)
colnames(matrix3) <- paste('Model', 1:4)

plot(rmse.ls1,type="o",xlim=c(0,10),ylim=c(0,250),xlab='folds',main='RMSE vs FOLDS',col='red')
par(new=TRUE)
points(rmse.ls2,type="o",col='blue')
points(rmse.lad1,type="o",col='green')
points(rmse.lad2,type="o",col='brown')

plot(mae.ls1,type="o",xlim=c(0,10),ylim=c(0,150),xlab='folds',main='MAE vs FOLDS',col='red')
par(new=TRUE)
points(mae.ls2,type="o",col='blue')
points(mae.lad1,type="o",col='green')
points(mae.lad2,type="o",col='brown')

plot(smdape.ls1,type="o",xlim=c(0,10),ylim=c(0,30),xlab='folds',main='SMDAPE vs FOLDS',col='red')
par(new=TRUE)
points(smdape.ls2,type="o",col='blue')
points(smdape.lad1,type="o",col='green')
points(smdape.lad2,type="o",col='brown')
