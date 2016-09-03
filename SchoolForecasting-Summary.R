####  ploting error distribution
## year2014-three year ahead without zoros

linear<-lm(X2013.first.grade~X2010.age.5+X2010.first.grade+X2009.first.grade)
l<-summary(linear)

Year2014three<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2011.age.5)+( l$coefficients[3,1]*X2011.first.grade)+( l$coefficients[4,1]*X2010.first.grade))/29)
D1<-ceiling(X2014.first.grade/29)-Year2014three
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
D2<-ceiling(X2014.first.grade/29)-ceiling(X2011.first.grade/29)
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
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("-4","-3", "-2", "-1","1","2","3","4"),ylim = c(0,0.25), beside = TRUE)
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)
## year2014-three year ahead zoros

D1<-ceiling(X2014.first.grade/29)-Year2014three
D22<-as.vector(D1)
D2<-ceiling(X2014.first.grade/29)-ceiling(X2011.first.grade/29)
D22<-as.vector(D2)
D11<-as.vector(D1)
D111<-D11[D11==0]
D222<-D22[D22==0]
K1<-length(which(!D11 == 0))
K2<-length(which(!D22 == 0))
P<-length(D222)
L<-length(D111)
height <- rbind(L/(sum(L)+K1), P/(sum(P)+K2))
height2<-rbind(L,P)
mp <- barplot(height,names.arg=c("mlr","naive"),ylim = c(0,1),xlim = c(0,5), beside = TRUE, space=c(0.5))
text(mp, height, labels = format(height2, 4),
     pos = 3, cex = .75)
#### error table 
####Regression2014 three year ahead
linear<-lm(X2013.first.grade~X2010.age.5+X2010.first.grade+X2009.first.grade)
l<-summary(linear)
Year2014three<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2011.age.5)+( l$coefficients[3,1]*X2011.first.grade)+( l$coefficients[4,1]*X2010.first.grade))/29)
Year2014three<-replace(Year2014three, Year2014three==-1, 0) 
D1<-ceiling(X2014.first.grade/29)- Year2014three
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result  0.3348891
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1625348
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
L9<-length(which(D11==0)) # 1823
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) # 0   0   4  11 265 392  63   9   3   1

####Naive 2014 three year ahead
D2<-ceiling(X2014.first.grade/29)-ceiling(X2011.first.grade/29)
D11<-c(D2)
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAE1<-mean(abs(D11))
MAE1 # result 0.3380008
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1664654
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
P9<-length(which(D11==0)) # 1811
P<-c(P11,P7,P3,P2,P1,P4,P5,P6,P8,P10) #[1]   0   3   4  53 477 193  23   5   2   0


#### Single regression tree 2014-three years ahead

fit <- rpart(X2014.first.grade~X2011.age.5+X2011.first.grade+X2010.first.grade+X2009.first.grade+X2010.age.5)
summary(fit)
prediction<-predict(fit)
p<-ceiling(prediction/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.420459
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.2052787
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
L9<-length(which(D11==0)) # 1716
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) #   1   2   8 105 429 242  52  10   3   2

#### Boosting tree 2014-three years ahead
fit <- gbm(X2014.first.grade~X2011.age.5+X2011.first.grade+X2010.first.grade+X2009.first.grade+X2010.age.5, distribution="gaussian")
summary(fit)
predictions <- predict(fit)
p<-ceiling(predictions/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 2.141968
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 1.189243s


