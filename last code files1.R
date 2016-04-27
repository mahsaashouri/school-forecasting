f<-read.csv("d:/Book1.csv",header = T)
head(f)
attach(f)

####  ploting error distribution
#Nick
## year2012-one year ahead without zoros
linear<-lm(first.grade.in.2012~age.5.in.2011+first.grade.in.2011+first.grade.in.2010)
l<-summary(linear)
Year2012one<-ceiling((l$coefficients[1,1]+(l$coefficients[2,1]*age.5.in.2011)+(l$coefficients[3,1]*first.grade.in.2011)+(l$coefficients[4,1]*first.grade.in.2010))/29)
D1<- ceiling(first.grade.in.2012/29)-Year2012one
D11<-as.vector(D1)
D111<-D11[!D11==0]
L1<-length(which(D111 == -1)) 
L2<-length(which(D111 == -2))
L3<-length(which(D111 == -3)) 
L4<-length(which(D111 == 1)) 
L5<-length(which(D111 == 2)) 
L6<-length(which(D111 == 3)) 
L<-c(L3,L2,L1,L4,L5,L6)
D2<-ceiling(first.grade.in.2012/29)-ceiling(first.grade.in.2011/29)
D22<-as.vector(D2)
D222<-D22[!D22==0]
P1<-length(which(D222 == -1)) 
P2<-length(which(D222 == -2)) 
P3<-length(which(D222 == -3)) 
P4<-length(which(D222 == 1)) 
P5<-length(which(D222 == 2)) 
P6<-length(which(D222 == 3)) 
P<-c(P3,P2,P1,P4,P5,P6)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("-3", "-2", "-1","1","2","3"),ylim  = c(0,450), beside = TRUE)
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2012-one year ahead zoros
D1<- ceiling(first.grade.in.2012/29)-Year2012one
D11<-as.vector(D1)
D111<-D11[D11==0]
L<-length(D111)
D2<-ceiling(first.grade.in.2012/29)-ceiling(first.grade.in.2011/29)
D22<-as.vector(D2)
D222<-D22[D22==0]
P<-length(D222)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("mlr","naive"), ylim=c(0,2300),beside = TRUE,space=c(0.2))
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2013-one year ahead without zoros

linear<-lm(first.grade.in.2013~age.5.in.2012+first.grade.in.2012+first.grade.in.2011)
l<-summary(linear)

Year2013one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*age.5.in.2012)+(l$coefficients[3,1]*first.grade.in.2012)+(l$coefficients[4,1]*first.grade.in.2011))/29)
D1<- ceiling(first.grade.in.2013/29)- Year2013one
D2<-ceiling(first.grade.in.2013/29)-ceiling(first.grade.in.2012/29)
D11<-as.vector(D1)
D111<-D11[!D11==0]
L1<-length(which(D111 == -1))
L2<-length(which(D111 == -2))
L3<-length(which(D111 == -3))
L4<-length(which(D111 == 1))
L5<-length(which(D111 == 2))
L6<-length(which(D111 == 3))
L7<-length(which(D111 == -4))
L<-c(L7,L3,L2,L1,L4,L5,L6)
D2<-ceiling(first.grade.in.2013/29)-ceiling(first.grade.in.2012/29)
D22<-as.vector(D2)
D222<-D22[!D22==0]
P1<-length(which(D222 == -1)) 
P2<-length(which(D222 == -2)) 
P3<-length(which(D222 == -3)) 
P4<-length(which(D222 == 1)) 
P5<-length(which(D222 == 2)) 
P6<-length(which(D222 == 3))
P7<-length(which(D222 == -4))
P<-c(P7,P3,P2,P1,P4,P5,P6)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("-4","-3", "-2", "-1","1","2","3"), ylim = c(0,400),beside = TRUE)
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2013-one year ahead zoros

D1<- ceiling(first.grade.in.2013/29)- Year2013one
D11<-as.vector(D1)
D111<-D11[D11==0]
L<-length(D111)
D2<-ceiling(first.grade.in.2013/29)-ceiling(first.grade.in.2012/29)
D22<-as.vector(D2)
D222<-D22[D22==0]
P<-length(D222)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("mlr","naive"),ylim = c(0,2300), beside = TRUE,space=c(0.2))
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2014-one year ahead without zoros

linear<-lm(first.grade.in.2014~age.5.in.2013+first.grade.in.2013+first.grade.in.2012)
l<-summary(linear)

Year2014one<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*age.5.in.2013)+(l$coefficients[3,1]*first.grade.in.2013)+(l$coefficients[4,1]*first.grade.in.2012))/29)
D1<- ceiling(first.grade.in.2014/29)- Year2014one
D11<-as.vector(D1)
D111<-D11[!D11==0]
L1<-length(which(D111 == -1)) 
L2<-length(which(D111 == -2)) 
L3<-length(which(D111 == -3)) 
L4<-length(which(D111 == 1)) 
L5<-length(which(D111 == 2)) 
L6<-length(which(D111 == 3)) 
L<-c(L3,L2,L1,L4,L5,L6)
D2<-ceiling(first.grade.in.2014/29)-ceiling(first.grade.in.2013/29)
D22<-as.vector(D2)
D222<-D22[!D22==0]
P1<-length(which(D222 == -1)) 
P2<-length(which(D222 == -2)) 
P3<-length(which(D222 == -3)) 
P4<-length(which(D222 == 1)) 
P5<-length(which(D222 == 2)) 
P6<-length(which(D222 == 3)) 
P<-c(P3,P2,P1,P4,P5,P6)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("-3", "-2", "-1","1","2","3"),ylim = c(0,400), beside = TRUE)
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2014-one year ahead zoros
D1<- ceiling(first.grade.in.2014/29)- Year2014one
D11<-as.vector(D1)
D111<-D11[D11==0]
L<-length(D111)
D2<-ceiling(first.grade.in.2014/29)-ceiling(first.grade.in.2013/29)
D22<-vector(length=2571)
D22<-as.vector(D2)
D222<-D22[D22==0]
P<-length(D222)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("mlr","naive"),ylim = c(0,2300), beside = TRUE ,space=c(0.2))
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2014-three year ahead without zoros

linear<-lm(first.grade.in.2014~age.5.in.2011+first.grade.in.2011+first.grade.in.2010)
l<-summary(linear)

Year2014three<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*age.5.in.2012)+( l$coefficients[3,1]*first.grade.in.2012)+( l$coefficients[4,1]*first.grade.in.2011))/29)
D1<-ceiling(first.grade.in.2014/29)-Year2014three
D11<-as.vector(D1)
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
D2<-ceiling(first.grade.in.2014/29)-ceiling(first.grade.in.2011/29)
D22<-as.vector(D2)
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
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("-4","-3", "-2", "-1","1","2","3","4"),ylim = c(0,550), beside = TRUE)
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
## year2014-three year ahead zoros

D1<-ceiling(first.grade.in.2014/29)-Year2014three
D22<-as.vector(D1)
D2<-ceiling(first.grade.in.2014/29)-ceiling(first.grade.in.2011/29)
D22<-as.vector(D2)
D11<-as.vector(D1)
D111<-D11[D11==0]
D222<-D22[D22==0]
P<-length(D222)
L<-length(D111)
height <- rbind(L, P)
mp <- barplot(height,names.arg=c("mlr","naive"),ylim = c(0,2300), beside = TRUE, space=c(0.2))
text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75)
#### error table ( table 4-page10)
####Regression2013 two year ahead
linear<-lm(first.grade.in.2013~age.5.in.2011+first.grade.in.2011+first.grade.in.2010)
l<-summary(linear)
Year2013two<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*age.5.in.2011)+( l$coefficients[3,1]*first.grade.in.2011)+( l$coefficients[4,1]*first.grade.in.2010))/29)
D1<-ceiling(first.grade.in.2013/29)- Year2013two
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result 0.2524085
MAPE1<-mean(abs(D11/(ceiling(first.grade.in.2013/29))))
D22<-c(ceiling(first.grade.in.2013/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.08034319

####Naive 2013 two year ahead
D2<-ceiling(first.grade.in.2013/29)-ceiling(first.grade.in.2011/29)
D11<-c(D2)
D22<-c(ceiling(first.grade.in.2013/29))
MAE1<-mean(abs(D11))
MAE1 # result 0.2878613
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.09725915

####Regression2014 two year ahead
linear<-lm(first.grade.in.2014~age.5.in.2012+first.grade.in.2012+first.grade.in.2011)
l<-summary(linear)
Year2014two<-ceiling((l$coefficients[1,1]+( l$coefficients[2,1]*age.5.in.2012)+( l$coefficients[3,1]*first.grade.in.2012)+( l$coefficients[4,1]*first.grade.in.2011))/29)
D1<-ceiling(first.grade.in.2014/29)-Year2014two
D11<-c(D1)
MAE1<-mean(abs(D11))
MAE1 # result  0.2593449
D22<-c(ceiling(first.grade.in.2014/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result  0.08512327

####Naive 2014 two year ahead

D2<-ceiling(first.grade.in.2014/29)-ceiling(first.grade.in.2012/29)
D11<-c(D2)
D22<-c(ceiling(first.grade.in.2014/29))
MAE1<-mean(abs(D11))
MAE1 # result 0.2581888
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.08314759

####Single regression tree 2013-two years ahead
library(rpart)
fit <- rpart(first.grade.in.2013~age.5.in.2011+first.grade.in.2011+first.grade.in.2010)
summary(fit)
plot(fit)
text(fit)
prediction<-predict(fit)
p<-ceiling(prediction/29)
error<-ceiling(first.grade.in.2013/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.4585742
D22<-c(ceiling(first.grade.in.2013/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1261205

#### Single regression tree 2014-two years ahead

fit <- rpart(first.grade.in.2014~age.5.in.2012+first.grade.in.2012+first.grade.in.2011)
summary(fit)
prediction<-predict(fit)
p<-ceiling(prediction/29)
error<-ceiling(first.grade.in.2014/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 0.3926782
D22<-c(ceiling(first.grade.in.2014/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 0.1314896

#### Boosting tree 2013-two years ahead

library(gbm)
library(survival)
library(lattice)
library(splines)
library(parallel)
fit <- gbm(first.grade.in.2013~age.5.in.2011+first.grade.in.2011+first.grade.in.2010, distribution="gaussian")
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
error<-ceiling(first.grade.in.2013/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 2.114451
D22<-c(ceiling(first.grade.in.2013/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 1.147641

#### Boosting tree 2014-two years ahead
fit <- gbm(first.grade.in.2014~age.5.in.2012+first.grade.in.2012+first.grade.in.2011, distribution="gaussian")
summary(fit)
predictions <- predict(fit)
p<-ceiling(predictions/29)
error<-ceiling(first.grade.in.2014/29)-p
D11<-c(error)
MAE1<-mean(abs(D11))
MAE1 # result 2.123314
D22<-c(ceiling(first.grade.in.2014/29))
MAPE1<-mean(abs(D11/D22))
MAPE1 # result 1.160386

#### coefficients in Table 5 p.11 report!
#2014 1-year-ahead
linear<-lm(first.grade.in.2014~age.5.in.2013+first.grade.in.2013+first.grade.in.2012)
l<-summary(linear)
#2014 2-year-ahead
linear<-lm(first.grade.in.2014~age.5.in.2012+first.grade.in.2012+first.grade.in.2011)
l<-summary(linear)
#2014 3-year-ahead
linear<-lm(first.grade.in.2014~age.5.in.2011+first.grade.in.2011+first.grade.in.2010)
l<-summary(linear)
#2014 4-year-ahead
linear<-lm(first.grade.in.2014~age.5.in.2010+first.grade.in.2010+first.grade.in.2009)
l<-summary(linear)

