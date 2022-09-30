#读入数据 
data(iris)
head(iris)

#创建贝叶斯分类模型
library(e1071)
classifier <- naiveBayes(iris[,c(1:4)],iris[,5])
classifier

#检查模型效果
result = predict(classifier,iris[,-5])
table(result,iris[,5])

#构造新数据并进行预测
newdata<-data.frame(Sepal.Length=4, Sepal.Width=2.5, Petal.Length=2.7, Petal.Width=1.2)
predict(classifier,newdata)
