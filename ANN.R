install.packages("caret")
install.packages("e1071")
library(nnet)
library(neuralnet)
library(caret)
library(e1071)
#데이터전처리 : 범주형 변수를 가변수로 변환
#수치형 : 표준화(scale)
#범주형 : 범주화(class.ind)
iris.df <- cbind(scale(iris[-5]),
                 class.ind(iris[,]$Species))
head(iris.df, 1)
set.seed(1)
train.index <- sample(c(1:dim(iris.df)[1]),
                      dim(iris.df)[1]*0.6)
train.df <- iris.df[train.index,]
vaild.df <- iris.df[-train.index,]
nn <- neuralnet(setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length, data=train.df, hidden=3)
nn$weights #가중치 점수 확인
plot(nn, rep="best")
#모델평가
train.pred <- compute(nn, train.df[,c(1:4)])
train.class <- apply(train.pred$net.result, 1, which.max)-1
train.class <- factor(train.class, level=c(0,1,2),
                      labels=c("setosa", "versicolor", "virginica"))
confusionMatrix(as.factor(train.class),
                as.factor(iris[train.index, ]$Species))

vaild.pred <- compute(nn, vaild.df[,c(1:4)])
vaild.class <- apply(vaild.pred$net.result, 1, which.max)-1
vaild.class <- factor(vaild.class, level=c(0,1,2),
                      labels=c("setosa", "versicolor", "virginica"))
confusionMatrix(as.factor(vaild.class),
                as.factor(iris[-train.index, ]$Species))
