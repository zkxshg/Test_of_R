#导入房产销售数据
library(nutshell)
data(sanfrancisco.home.sales)
head(sanfrancisco.home.sales)

#划分训练集和测试集
nrow(sanfrancisco.home.sales) * .7
#取得训练集和测试集的位置
sanfrancisco.home.sales.training.indices <- sample(1:nrow(sanfrancisco.home.sales),2296)
sanfrancisco.home.sales.testing.indices <- setdiff(rownames(sanfrancisco.home.sales),sanfrancisco.home.sales.training.indices)
#基于位置划分数据
sanfrancisco.home.sales.training <-sanfrancisco.home.sales[sanfrancisco.home.sales.training.indices,]
sanfrancisco.home.sales.testing <- sanfrancisco.home.sales[sanfrancisco.home.sales.testing.indices,]

#定义误差分析函数
calculate_rms_error <- function(mdl, train, test, yval) {
train.yhat <- predict(object=mdl,newdata=train)
test.yhat <- predict(object=mdl,newdata=test)
train.y <- with(train,get(yval))
test.y <- with(test,get(yval))
train.err <- sqrt(mean((train.yhat - train.y)^2))
test.err <- sqrt(mean((test.yhat - test.y)^2))
c(train.err=train.err,test.err=test.err)
}

#=================创建神经网络模型=========================
library(nnet)

sf.price.model.nnet <- nnet(
price~bedrooms+squarefeet+lotsize+latitude+
longitude+neighborhood+month,
data=sanfrancisco.home.sales.training, size=12,
skip=TRUE, linout=TRUE, decay=0.025, na.action=na.omit)

#概述神经网络模型
summary(sf.price.model.nnet)
print(sf.price.model.nnet)
calculate_rms_error(sf.price.model.nnet,
na.omit(sanfrancisco.home.sales.training),
na.omit(sanfrancisco.home.sales.testing),
"price")

#================创建支持向量机模型======================
library(e1071)
f.price.model.svm <- svm(
price~bedrooms+squarefeet+lotsize+latitude+
longitude+neighborhood+month,
data=sanfrancisco.home.sales.training)

#概览支持向量机
calculate_rms_error(f.price.model.svm,
na.omit(sanfrancisco.home.sales.training),
na.omit(sanfrancisco.home.sales.testing),
"price")
