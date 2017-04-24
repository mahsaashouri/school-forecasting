#reading data file.
f<-read.csv("d:/ForecastingSchool.csv",header = T)
head(f)
attach(f)

library(e1071) ##for svm
library(rpart)## for tree
library(randomForest)## for random forest

set.seed(123) ## setting seed for repeating the results
####  ploting error distribution

## year2012-one year ahead without zoros

## Regression results

linear<-lm(X2011.first.grade~X2010.age.5+X2010.first.grade+X2009.first.grade)
l<-summary(linear)
Year2012one<-ceiling((l$coefficients[1,1]+(l$coefficients[2,1]*X2011.age.5)+(l$coefficients[3,1]*X2011.first.grade)+(l$coefficients[4,1]*X2010.first.grade))/29)
D1<- ceiling(X2012.first.grade/29)-Year2012one
D11<-as.vector(D1)
K1<-length(which(D11 == 0)) 
D111<-D11[!D11==0]
L1<-length(which(D111 == -1)) 
L2<-length(which(D111 == -2))
L3<-length(which(D111 == -3)) 
L4<-length(which(D111 == 1)) 
L5<-length(which(D111 == 2)) 
L6<-length(which(D111 == 3)) 
L<-c(L3,L2,L1,L4,L5,L6)

#### Naive Resuts
D2<-ceiling(X2012.first.grade/29)-ceiling(X2011.first.grade/29)
D22<-as.vector(D2)
K2<-length(which(D22 == 0)) 
D222<-D22[!D22==0]
P1<-length(which(D222 == -1)) 
P2<-length(which(D222 == -2)) 
P3<-length(which(D222 == -3)) 
P4<-length(which(D222 == 1)) 
P5<-length(which(D222 == 2)) 
P6<-length(which(D222 == 3)) 
P<-c(P3,P2,P1,P4,P5,P6)

#### Ploting Results
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("-3", "-2", "-1","1","2","3"),ylim  = c(0,0.2), beside = TRUE)
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)

## year2012-one year ahead zoros
#### Regression
D1<- ceiling(X2012.first.grade/29)-Year2012one
D11<-as.vector(D1)
K1<-length(which(!D11 == 0))
D111<-D11[D11==0]
L<-length(D111)

#### Naive
D2<-ceiling(X2012.first.grade/29)-ceiling(X2011.first.grade/29)
D22<-as.vector(D2)
K2<-length(which(!D22 == 0))
D222<-D22[D22==0]
P<-length(D222)

#### Ploting Results
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("mlr","naive","SVM"), ylim=c(0,1),xlim = c(0,5),beside = TRUE,space=c(0.5))
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)

## year2013-one year ahead without zoros

## Regression Results

linear<-lm(X2012.first.grade~X2011.age.5+X2011.first.grade+X2010.first.grade)
l<-summary(linear)

Year2013one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2012.age.5)+(l$coefficients[3,1]*X2012.first.grade)+(l$coefficients[4,1]*X2011.first.grade))/29)
D1<- ceiling(X2013.first.grade/29)- Year2013one
D11<-as.vector(D1)
K1<-length(which(D11 == 0))
D111<-D11[!D11==0]
L1<-length(which(D111 == -1))
L2<-length(which(D111 == -2))
L3<-length(which(D111 == -3))
L4<-length(which(D111 == 1))
L5<-length(which(D111 == 2))
L6<-length(which(D111 == 3))
L7<-length(which(D111 == -4))
L8<-length(which(D111 == 4))
L<-c(L7,L3,L2,L1,L4,L5,L6,L8)
L

#### Naive Results
D2<-ceiling(X2013.first.grade/29)-ceiling(X2012.first.grade/29)
D22<-as.vector(D2)
K2<-length(which(D22 == 0)) 
D222<-D22[!D22==0]
P1<-length(which(D222 == -1)) 
P2<-length(which(D222 == -2)) 
P3<-length(which(D222 == -3)) 
P4<-length(which(D222 == 1)) 
P5<-length(which(D222 == 2)) 
P6<-length(which(D222 == 3))
P7<-length(which(D222 == -4))
P8<-length(which(D222 == 4))
P<-c(P7,P3,P2,P1,P4,P5,P6,P8)

#### Ploting Results
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("-4","-3", "-2", "-1","1","2","3","4"), ylim = c(0,0.2),beside = TRUE)
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)
## year2013-one year ahead zoros
#### Regression
D1<- ceiling(X2013.first.grade/29)-Year2013one
D11<-as.vector(D1)
K1<-length(which(!D11 == 0))
D111<-D11[D11==0]
L<-length(D111)

#### Naive
D2<-ceiling(X2013.first.grade/29)-ceiling(X2012.first.grade/29)
D22<-as.vector(D2)
K2<-length(which(!D22 == 0))
D222<-D22[D22==0]
P<-length(D222)

#### Ploting Results
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("mlr","naive","SVM"), ylim=c(0,1),xlim = c(0,5),beside = TRUE,space=c(0.5))
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)


## year2014-one year ahead without zoros

## Regression Results

linear<-lm(X2013.first.grade~X2012.age.5+X2012.first.grade+X2011.first.grade)
l<-summary(linear)

Year2014one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2013.age.5)+(l$coefficients[3,1]*X2013.first.grade)+(l$coefficients[4,1]*X2012.first.grade))/29)
D1<- ceiling(X2014.first.grade/29)- Year2014one
D11<-as.vector(D1)
K1<-length(which(D11 == 0))
D111<-D11[!D11==0]
L1<-length(which(D111 == -1))
L2<-length(which(D111 == -2))
L3<-length(which(D111 == -3))
L4<-length(which(D111 == 1))
L5<-length(which(D111 == 2))
L6<-length(which(D111 == 3))
L7<-length(which(D111 == -4))
L8<-length(which(D111 == 4))
L<-c(L7,L3,L2,L1,L4,L5,L6,L8)
L

#### Naive Results
D2<-ceiling(X2014.first.grade/29)-ceiling(X2013.first.grade/29)
D22<-as.vector(D2)
K2<-length(which(D22 == 0)) 
D222<-D22[!D22==0]
P1<-length(which(D222 == -1)) 
P2<-length(which(D222 == -2)) 
P3<-length(which(D222 == -3)) 
P4<-length(which(D222 == 1)) 
P5<-length(which(D222 == 2)) 
P6<-length(which(D222 == 3))
P7<-length(which(D222 == -4))
P8<-length(which(D222 == 4))
P<-c(P7,P3,P2,P1,P4,P5,P6,P8)

#### Ploting Results
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("-4","-3", "-2", "-1","1","2","3","4"), ylim = c(0,0.2),beside = TRUE)
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)
## year2013-one year ahead zoros
#### Regression
D1<- ceiling(X2014.first.grade/29)-Year2014one
D11<-as.vector(D1)
K1<-length(which(!D11 == 0))
D111<-D11[D11==0]
L<-length(D111)

#### Naive
D2<-ceiling(X2014.first.grade/29)-ceiling(X2013.first.grade/29)
D22<-as.vector(D2)
K2<-length(which(!D22 == 0))
D222<-D22[D22==0]
P<-length(D222)

#### Ploting Results
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("mlr","naive","SVM"), ylim=c(0,1),xlim = c(0,5),beside = TRUE,space=c(0.5))
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)


#######################################################################################
#### error table 
####Regression2012 one year ahead
linear<-lm(X2011.first.grade~X2010.age.5+X2010.first.grade+X2009.first.grade)
l<-summary(linear)
Year2012one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2011.age.5)+( l$coefficients[3,1]*X2011.first.grade)+( l$coefficients[4,1]*X2010.first.grade))/29)
Year2012one<-replace(Year2012one, Year2012one<=0, 0) 
D1<-ceiling(X2012.first.grade/29)-Year2012one
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result    0.2248152
V1<-as.vector(X2012.first.grade)
V2<-ceiling(V1/29)
D22<-V2[!V2==0]
MAPE1<-mean(abs(D11/D22))
MAPE1 # result  0.08492137
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) #  2027
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) # 0   0   0  11 255 256  21   1   0   0

####SVM 2012 one year ahead - Not including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade) , y= f$X2011.first.grade)
model <- svm(y~., train)
summary(model)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade) , y= f$X2012.first.grade)
Year2012onesvm=predict (model, newdata =datanew)
Year2012onesvm<-replace(Year2012onesvm, Year2012onesvm<=0, 0) 
F1<- ceiling(X2012.first.grade/29)-ceiling(Year2012onesvm/29)
F11<-c(F1)
MAE1<-mean(abs(F11))
MAE1 # result    0.2201478
V1<-as.vector(X2012.first.grade)
V2<-ceiling(V1/29)
F22<-V2[!V2==0]
MAPE1<-mean(abs(F11/F22))
MAPE1 # result   0.07738719
F11<-as.vector(F1)
J1<-length(which(F11 == 0)) 
F111<-F11[!F11==0]
U1<-length(which(F111 == -1)) 
U2<-length(which(F111 == -2))
U3<-length(which(F111 == -3)) 
U4<-length(which(F111 == 1)) 
U5<-length(which(F111 == 2)) 
U6<-length(which(F111 == 3)) 
U7<-length(which(F11 == 4))
U8<-length(which(F11 == -4))
U10<-length(which(F11 == 5))
U11<-length(which(F11 == -5))
U9<-length(which(F11==0))
U9 # 2044
U<-c(U11,U8,U3,U2,U1,U4,U5,U6,U7,U10)
U #  0   0   0   7 216 278  22   3   0   1

####SVM 2012 one year ahead - Including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade) , y= f$X2011.first.grade)
model <- svm(y~., train)
summary(model)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5) , y= f$X2012.first.grade)
Year2012onesvm=predict (model, newdata =datanew)
Year2012onesvm<-replace(Year2012onesvm, Year2012onesvm<=0, 0) 
F1<- ceiling(X2012.first.grade/29)-ceiling(Year2012onesvm/29)
F11<-c(F1)
MAE1<-mean(abs(F11))
MAE1 # result    0.2201478
V1<-as.vector(X2012.first.grade)
V2<-ceiling(V1/29)
F22<-V2[!V2==0]
MAPE1<-mean(abs(F11/F22))
MAPE1 # result 0.07738719
F11<-as.vector(F1)
J1<-length(which(F11 == 0)) 
F111<-F11[!F11==0]
U1<-length(which(F111 == -1)) 
U2<-length(which(F111 == -2))
U3<-length(which(F111 == -3)) 
U4<-length(which(F111 == 1)) 
U5<-length(which(F111 == 2)) 
U6<-length(which(F111 == 3)) 
U7<-length(which(F11 == 4))
U8<-length(which(F11 == -4))
U10<-length(which(F11 == 5))
U11<-length(which(F11 == -5))
U9<-length(which(F11==0))
U9 # 2044
U<-c(U11,U8,U3,U2,U1,U4,U5,U6,U7,U10)
U #   0   0   0   7 216 278  22   3   0   1
####Naive 2012 one year ahead

D2<-ceiling(X2012.first.grade/29)-ceiling(X2011.first.grade/29)
D11<-c(D2)
V1<-as.vector(X2012.first.grade)
V2<-ceiling(V1/29)
D22<-V2[!V2==0]
MAE1<-mean(abs(D11))
MAE1 # result 0.2590432
MAPE1<-mean(abs(D11/D22))
MAPE1 # result  0.1018189
P1<-length(which(D11 == -1)) 
P2<-length(which(D11 == -2)) 
P3<-length(which(D11 == -3)) 
P4<-length(which(D11 == 1)) 
P5<-length(which(D11 == 2)) 
P6<-length(which(D11 == 3)) 
P7<-length(which(D11 == -4))
P8<-length(which(D11 == 4))
P10<-length(which(D11 == 5))
P11<-length(which(D11 == -5))
P9<-length(which(D11==0)) 
P9 # 1949
P<-c(P11,P7,P3,P2,P1,P4,P5,P6,P8,P10)
P # 0   0   2  30 387 194   8   1   0   0

####Single regression tree 2012-one year ahead - Including all past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade) , y= f$X2011.first.grade)
fit <- rpart(y~.,data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5) , y= f$X2012.first.grade)
prediction<-predict(fit,data=datanew)
p<-ceiling(prediction/29)
error<-ceiling(X2012.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4107351
V1<-as.vector(X2012.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1470511
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 # 1680
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L # 1   1   5  36 252 508  67  19   1   1

####Single regression tree 2012-one year ahead - Not including all past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade) , y= f$X2011.first.grade)
fit <- rpart(y~.,data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade) , y= f$X2012.first.grade)
summary(fit)
plot(fit)
text(fit)
prediction<-predict(fit,data=datanew)
p<-ceiling(prediction/29)
error<-ceiling(X2012.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4107351
V1<-as.vector(X2012.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1470511
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 # 1680
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L # 1   1   5  36 252 508  67  19   1   1

########### Random Forest 2012-one year ahead - Including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade) , y= f$X2011.first.grade)
fit <- randomForest(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5) , y= f$X2012.first.grade)

prediction<-predict(fit,datanew)

p<-ceiling(prediction/29)
error<-ceiling(X2012.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result  0.2333722
V1<-as.vector(X2012.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.08256634

L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 #  2016
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L #  0   0   0  10 240 274  27   4   0   0

########### Random Forest 2012-one year ahead - Not including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade) , y= f$X2011.first.grade)
fit <- randomForest(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade) , y= f$X2012.first.grade)

prediction<-predict(fit,datanew)

p<-ceiling(prediction/29)
error<-ceiling(X2012.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result  0.2372618
V1<-as.vector(X2012.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.08480988

L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 #  2003
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L #  0   0   0   9 244 286  25   4   0   0

####Regression2013 one year ahead

linear<-lm(X2012.first.grade~X2011.age.5+X2011.first.grade+X2010.first.grade)
l<-summary(linear)
Year2013one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2012.age.5)+( l$coefficients[3,1]*X2012.first.grade)+( l$coefficients[4,1]*X2011.first.grade))/29)
Year2013one<-replace(Year2013one, Year2013one<=0, 0) 
D1<-ceiling(X2013.first.grade/29)- Year2013one
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result   0.2279269
V1<-as.vector(X2013.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1022326
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) 
L9 # 2013
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) 
L # 0   1   0   3 253 280  20   1   0   0

####SVM 2013 one year ahead - Not including all the past data
train<-data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade) , y= f$X2012.first.grade)
model <- svm(y~., train)
summary(model)
datanew=data.frame(x=cbind(f$X2012.age.5,f$X2012.first.grade,f$X2011.first.grade) , y= f$X2013.first.grade)
Year2013onesvm=predict (model, newdata =datanew)
Year2013onesvm<-replace(Year2013onesvm, Year2013onesvm<=0, 0) 
F1<- ceiling(X2013.first.grade/29)-ceiling(Year2013onesvm/29)
F11<-c(F1)
MAE1<-mean(abs(F11))
MAE1 # result   0.2407623
V1<-as.vector(X2013.first.grade)
V2<-ceiling(V1/29)
F22<-V2[!V2==0]
MAPE1<-mean(abs(F11/F22))
MAPE1 # result  0.1057757
F11<-as.vector(F1)
J1<-length(which(F11 == 0)) 
F111<-F11[!F11==0]
U1<-length(which(F111 == -1)) 
U2<-length(which(F111 == -2))
U3<-length(which(F111 == -3)) 
U4<-length(which(F111 == 1)) 
U5<-length(which(F111 == 2)) 
U6<-length(which(F111 == 3))
U7<-length(which(F111 == 4))
U8<-length(which(F111 == -4))
U10<-length(which(F11 == 5))
U11<-length(which(F11 == -5))
U9<-length(which(F11==0))
U9 #  1998
U<-c(U11,U8,U3,U2,U1,U4,U5,U6,U7,U10)
U # 0   1   0   7 244 296  20   3   1   0

####SVM 2013 one year ahead - Including all the past data
train<-data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2010.age.5,f$X2009.first.grade) , y= f$X2012.first.grade)
model <- svm(y~., train)
summary(model)
datanew=data.frame(x=cbind(f$X2012.age.5,f$X2012.first.grade,f$X2011.first.grade,f$X2011.age.5,f$X2010.first.grade,f$X2010.age.5,f$X2009.first.grade) , y= f$X2013.first.grade)
Year2013onesvm=predict (model, newdata =datanew)
Year2013onesvm<-replace(Year2013onesvm, Year2013onesvm<=0, 0) 
F1<- ceiling(X2013.first.grade/29)-ceiling(Year2013onesvm/29)
F11<-c(F1)
MAE1<-mean(abs(F11))
MAE1 # result    0.2306496
V1<-as.vector(X2013.first.grade)
V2<-ceiling(V1/29)
F22<-V2[!V2==0]
MAPE1<-mean(abs(F11/F22))
MAPE1 # result  0.09599696
F11<-as.vector(F1)
J1<-length(which(F11 == 0)) 
F111<-F11[!F11==0]
U1<-length(which(F111 == -1)) 
U2<-length(which(F111 == -2))
U3<-length(which(F111 == -3)) 
U4<-length(which(F111 == 1)) 
U5<-length(which(F111 == 2)) 
U6<-length(which(F111 == 3))
U7<-length(which(F111 == 4))
U8<-length(which(F111 == -4))
U10<-length(which(F11 == 5))
U11<-length(which(F11 == -5))
U9<-length(which(F11==0))
U9 #  2031
U<-c(U11,U8,U3,U2,U1,U4,U5,U6,U7,U10)
U # 0   1   0   3 224 277  29   4   1   0
####Naive 2013 one year ahead
D2<-ceiling(X2013.first.grade/29)-ceiling(X2012.first.grade/29)
D11<-c(D2)
V1<-as.vector(X2013.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAE1<-mean(abs(D11))
MAE1 # result 0.2500972
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1154394
P1<-length(which(D11 == -1)) 
P2<-length(which(D11 == -2)) 
P3<-length(which(D11 == -3)) 
P4<-length(which(D11 == 1)) 
P5<-length(which(D11 == 2)) 
P6<-length(which(D11 == 3)) 
P7<-length(which(D11 == -4))
P8<-length(which(D11 == 4))
P10<-length(which(D11 == 5))
P11<-length(which(D11 == -5))
P9<-length(which(D11==0)) 
P9 #  1955
P<-c(P11,P7,P3,P2,P1,P4,P5,P6,P8,P10)
P #   0   1   0  13 338 253  11   0   0   0
####Single regression tree 2013-one years ahead - Including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade,f$X2011.age.5,f$X2011.first.grade) , y= f$X2012.first.grade)
fit <- rpart(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5,f$X2012.first.grade,f$X2012.age.5) , y= f$X2013.first.grade)
summary(fit)
plot(fit)
text(fit)
prediction<-predict(fit,data=datanew)
p<-ceiling(prediction/29)
error<-ceiling(X2013.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4216258
V1<-as.vector(X2013.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1897387
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) 
L9 # 1654
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L # 0   2   6  62 344 439  46  15   2   0

####Single regression tree 2013-one years ahead - Not including all the past data
train<-data.frame(x=cbind(f$X2010.first.grade,f$X2011.age.5,f$X2011.first.grade) , y= f$X2012.first.grade)
fit <- rpart(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.first.grade,f$X2012.age.5,f$X2010.first.grade) , y= f$X2013.first.grade)
summary(fit)
plot(fit)
text(fit)
prediction<-predict(fit,data=datanew)
p<-ceiling(prediction/29)
error<-ceiling(X2013.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4216258
V1<-as.vector(X2013.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1897387
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) 
L9 # 1654
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L # 0   2   6  62 344 439  46  15   2   0

########### Random Forest 2013-one year ahead -Including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade,f$X2011.age.5,f$X2011.first.grade) , y= f$X2012.first.grade)
fit <- randomForest(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5,f$X2012.first.grade,f$X2012.age.5) , y= f$X2013.first.grade)

prediction<-predict(fit,datanew)

p<-ceiling(prediction/29)
error<-ceiling(X2013.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result   0.3434461
V1<-as.vector(X2013.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.149394

L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 #  1821
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L #  1   0   4  39 413 234  41  13   5   0

########### Random Forest 2013-one year ahead -Not including all the past data
train<-data.frame(x=cbind(f$X2010.first.grade,f$X2011.age.5,f$X2011.first.grade) , y= f$X2012.first.grade)
fit <- randomForest(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.first.grade,f$X2012.age.5,f$X2012.first.grade) , y= f$X2013.first.grade)

prediction<-predict(fit,datanew)

p<-ceiling(prediction/29)
error<-ceiling(X2013.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result   0.2364839
V1<-as.vector(X2013.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1017984

L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 #  2004
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L #  0   1   0   4 237 296  27   0   1   1
####Regression2014 one year ahead
linear<-lm(X2013.first.grade~X2012.age.5+X2012.first.grade+X2011.first.grade)
l<-summary(linear)
Year2014one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2013.age.5)+( l$coefficients[3,1]*X2013.first.grade)+( l$coefficients[4,1]*X2012.first.grade))/29)
Year2014one<-replace(Year2014one, Year2014one<=0, 0) 
D1<-ceiling(X2014.first.grade/29)- Year2014one
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result 0.2275379
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1119028
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) 
L9 # 2027
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) 
L # 0   0   1  10 226 282  21   4   0   0

####SVM 2014 one year ahead - Not including all the past data
train=data.frame(x=cbind(f$X2012.age.5,f$X2012.first.grade,f$X2011.first.grade) , y= f$X2013.first.grade)
model <- svm(y~., train)
summary(model)
datanew=data.frame(x=cbind(f$X2013.age.5,f$X2013.first.grade,f$X2012.first.grade) , y= f$X2014.first.grade)
Year2014onesvm=predict (model, newdata =datanew)
Year2014onesvm<-replace(Year2014onesvm, Year2014onesvm<=0, 0) 
F1<- ceiling(X2014.first.grade/29)-ceiling(Year2014onesvm/29)
F11<-c(F1)
MAE1<-mean(abs(F11))
MAE1 # result     0.2306496
V1<-as.vector(X2014.first.grade)
V2<-ceiling(V1/29)
F22<-V2[!V2==0]
MAPE1<-mean(abs(F11/F22))
MAPE1 # result  0.1067853
F11<-as.vector(F1)
J1<-length(which(F11 == 0)) 
F111<-F11[!F11==0]
U1<-length(which(F111 == -1)) 
U2<-length(which(F111 == -2))
U3<-length(which(F111 == -3)) 
U4<-length(which(F111 == 1)) 
U5<-length(which(F111 == 2)) 
U6<-length(which(F111 == 3))
U7<-length(which(F111 == 4))
U8<-length(which(F111 == -4))
U10<-length(which(F11 == 5))
U11<-length(which(F11 == -5))
U9<-length(which(F11==0))
U9 #  2029
U<-c(U11,U8,U3,U2,U1,U4,U5,U6,U7,U10)
U # 0   0   1   6 199 301  30   4   0   0

####SVM 2014 one year ahead - Including all the past data
train=data.frame(x=cbind(f$X2012.age.5,f$X2012.first.grade,f$X2011.first.grade,f$X2011.age.5,f$X2010.first.grade,f$X2010.age.5,f$X2009.first.grade) , y= f$X2013.first.grade)
model <- svm(y~., train)
summary(model)
datanew=data.frame(x=cbind(f$X2013.age.5,f$X2013.first.grade,f$X2012.first.grade,f$X2012.age.5,f$X2011.first.grade,f$X2011.age.5,f$X2010.first.grade,f$X2010.age.5,f$X2009.first.grade) , y= f$X2014.first.grade)
Year2014onesvm=predict (model, newdata =datanew)
Year2014onesvm<-replace(Year2014onesvm, Year2014onesvm<=0, 0) 
F1<- ceiling(X2014.first.grade/29)-ceiling(Year2014onesvm/29)
F11<-c(F1)
MAE1<-mean(abs(F11))
MAE1 # result     0.2154804
V1<-as.vector(X2014.first.grade)
V2<-ceiling(V1/29)
F22<-V2[!V2==0]
MAPE1<-mean(abs(F11/F22))
MAPE1 # result  0.09866047
F11<-as.vector(F1)
J1<-length(which(F11 == 0)) 
F111<-F11[!F11==0]
U1<-length(which(F111 == -1)) 
U2<-length(which(F111 == -2))
U3<-length(which(F111 == -3)) 
U4<-length(which(F111 == 1)) 
U5<-length(which(F111 == 2)) 
U6<-length(which(F111 == 3))
U7<-length(which(F111 == 4))
U8<-length(which(F111 == -4))
U10<-length(which(F11 == 5))
U11<-length(which(F11 == -5))
U9<-length(which(F11==0))
U9 #  2063
U<-c(U11,U8,U3,U2,U1,U4,U5,U6,U7,U10)
U # 0   0   1   6 195 277  25   2   0   1
####Naive 2014 three year ahead
D2<-ceiling(X2014.first.grade/29)-ceiling(X2013.first.grade/29)
D11<-c(D2)
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAE1<-mean(abs(D11))
MAE1 # result 0.2481525
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1216809
P1<-length(which(D11 == -1)) 
P2<-length(which(D11 == -2)) 
P3<-length(which(D11 == -3)) 
P4<-length(which(D11 == 1)) 
P5<-length(which(D11 == 2)) 
P6<-length(which(D11 == 3)) 
P7<-length(which(D11 == -4))
P8<-length(which(D11 == 4))
P10<-length(which(D11 == 5))
P11<-length(which(D11 == -5))
P9<-length(which(D11==0))
P9 #  1988
P<-c(P11,P7,P3,P2,P1,P4,P5,P6,P8,P10) 
P #     0   0   4  16 281 253  27   2   0   0

#### Single regression tree 2014-one years ahead -Including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade,f$X2011.age.5,f$X2011.first.grade,f$X2012.first.grade,f$X2012.age.5) , y= f$X2013.first.grade)
fit <- rpart(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5,f$X2012.first.grade,f$X2012.age.5,f$X2013.first.grade,f$X2013.age.5) , y= f$X2014.first.grade)

summary(fit)
prediction<-predict(fit,data=datanew)
p<-ceiling(prediction/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4041229
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1861741
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) 
L9 # 1705
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) 
L #    0   1   8  47 238 483  75  11   2   1
#### Single regression tree 2014-one years ahead -Not including all the past data
train<-data.frame(x=cbind(f$X2011.first.grade,f$X2012.first.grade,f$X2012.age.5) , y= f$X2013.first.grade)
fit <- rpart(y~., data = train)
datanew=data.frame(x=cbind(f$X2012.first.grade,f$X2013.first.grade,f$X2013.age.5) , y= f$X2014.first.grade)

summary(fit)
prediction<-predict(fit,data=datanew)
p<-ceiling(prediction/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4041229
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1861741
L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0)) 
L9 # 1705
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) 
L #    0   1   8  47 238 483  75  11   2   1
########### Random Forest 2014-one year ahead - Including all the past data
train<-data.frame(x=cbind(f$X2010.age.5,f$X2010.first.grade,f$X2009.first.grade,f$X2011.age.5,f$X2011.first.grade,f$X2012.first.grade,f$X2012.age.5) , y= f$X2013.first.grade)
fit <- randomForest(y~., data = train)
datanew=data.frame(x=cbind(f$X2011.age.5,f$X2011.first.grade,f$X2010.first.grade,f$X2009.first.grade,f$X2010.age.5,f$X2012.first.grade,f$X2012.age.5,f$X2013.first.grade,f$X2013.age.5) , y= f$X2014.first.grade)

prediction<-predict(fit,datanew)

p<-ceiling(prediction/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result   0.3290548
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1564644

L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 #  1837
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L #  0   0   3  27 361 286  41  11   4   1
########### Random Forest 2014-one year ahead - Not including all the past data
train<-data.frame(x=cbind(f$X2011.first.grade,f$X2012.first.grade,f$X2012.age.5) , y= f$X2013.first.grade)
fit <- randomForest(y~., data = train)
datanew=data.frame(x=cbind(f$X2012.first.grade,f$X2013.first.grade,f$X2013.age.5) , y= f$X2014.first.grade)

prediction<-predict(fit,datanew)

p<-ceiling(prediction/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result   0.235706
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1075695

L1<-length(which(D11 == -1)) 
L2<-length(which(D11 == -2)) 
L3<-length(which(D11 == -3)) 
L4<-length(which(D11 == 1)) 
L5<-length(which(D11 == 2)) 
L6<-length(which(D11 == 3))
L7<-length(which(D11 == -4))
L8<-length(which(D11 == 4))
L10<-length(which(D11 == 5))
L11<-length(which(D11 == -5))
L9<-length(which(D11==0))
L9 #  2013
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10)
L # 0   0   0  11 221 295  26   4   1   0

####### Chi-square tests
################################ combining last small errors (positives and negatives)
one_year_ahead_2012<-rbind(c(32,387,1949,194,9),c(11,255,2027,256,22))
chisq.test(one_year_ahead_2012,correct = TRUE)
one_year_ahead_2013<-rbind(c(14,338,1955,253,11),c(4,253,2013,280,21))
chisq.test(one_year_ahead_2013,correct=TRUE)
one_year_ahead_2014<-rbind(c(20,281,1988,253,29),c(11,226,2027,282,25))
chisq.test(one_year_ahead_2014,correct = TRUE)
three_year_ahed_2014<-rbind(c(7,53,477,1811,193,23,7),c(4,11,265,1823,392,63,13))
chisq.test(three_year_ahed_2014,correct = TRUE)

#### coefficients tables
#2014 1-year-ahead
linear<-lm(X2014.first.grade~X2013.age.5+X2013.first.grade+X2012.first.grade)
l<-summary(linear)
l#2014 2-year-ahead
linear<-lm(X2014.first.grade~X2012.age.5+X2012.first.grade+X2011.first.grade)
l<-summary(linear)
#2014 3-year-aheads
linear<-lm(X2014.first.grade~X2011.age.5+X2011.first.grade+X2010.first.grade)
l<-summary(linear)
#2014 4-year-ahead
linear<-lm(X2014.first.grade~X2010.age.5+X2010.first.grade+X2009.first.grade)
l<-summary(linear)





