#reading data file.
f<-read.csv("d:/ForecastingSchool.csv",header = T)
head(f)
attach(f)

####  ploting error distribution
## year2014-three year ahead without zoros

linear<-lm(X2014.first.grade~X2011.age.5+X2011.first.grade+X2010.first.grade)
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
####Regression2014 one year ahead
linear<-lm(X2014.first.grade~X2013.age.5+X2013.first.grade+X2012.first.grade)
l<-summary(linear)
Year2014one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*X2013.age.5)+( l$coefficients[3,1]*X2013.first.grade)+( l$coefficients[4,1]*X2012.first.grade))/29)
# we can use predict function as well"predict(linear)", you will get same answer
Year2014one<-replace(Year2014one, Year2014one<=0, 0) 
D1<-ceiling(X2014.first.grade/29)-Year2014one
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result    0.2228705
V1<-as.vector(X2014.first.grade)
V2<-ceiling(V1/29)
D22<-V2[!V2==0]
MAPE1<-mean(abs(D11/D22))
MAPE1 # result  0.1119277
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
L9<-length(which(D11==0)) # 2035
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) # 0   0   1  13 269 234  16   3   0   0

####Naive 2014 one year ahead

D2<-ceiling(X2014.first.grade/29)-ceiling(X2013.first.grade/29)
D11<-c(D2)
V1<-as.vector(X2014.first.grade)
V2<-ceiling(V1/29)
D22<-V2[!V2==0]
MAE1<-mean(abs(D11))
MAE1 # result 0.2481525
MAPE1<-mean(abs(D11/D22))
MAPE1 # result  0.1216809
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
P9<-length(which(D11==0)) # 1988
P<-c(P11,P7,P3,P2,P1,P4,P5,P6,P8,P10) # 0   0   4  16 281 253  27   2   0   0
#### Single regression tree 2014-one year ahead

fit <- rpart(X2014.first.grade~X2013.age.5+X2012.age.5+X2011.age.5+X2010.age.5+X2013.first.grade+X2012.first.grade+X2011.first.grade+X2010.first.grade+X2009.first.grade)
summary(fit)
prediction<-predict(fit)
p<-ceiling(prediction/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.3788409
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1838606
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
L9<-length(which(D11==0)) # 1754
L<-c(L11,L7,L3,L2,L1,L4,L5,L6,L8,L10) #   0   0   4  84 453 222  47   5   1   0

#### Boosting tree 2014-one years ahead
fit <- gbm(X2014.first.grade~X2013.age.5+X2012.age.5+X2011.age.5+X2010.age.5+X2013.first.grade+X2012.first.grade+X2011.first.grade+X2010.first.grade+X2009.first.grade, distribution="gaussian")
summary(fit)
predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}
predictions <- predict(fit)
p<-ceiling(predictions/29)
error<-ceiling(X2014.first.grade/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 2.14119
V1<-as.vector(X2014.first.grade)
V<-V1[!V1==0]
D22<-c(ceiling(V/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 1.190669

#### coefficients tables!
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


