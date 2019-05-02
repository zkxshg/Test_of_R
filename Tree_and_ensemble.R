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
#保存划分结果
save(sanfrancisco.home.sales.training.indices,sanfrancisco.home.sales.testing.indices,
sanfrancisco.home.sales,file="~/sanfrancisco.home.sales.RData")

#============建立回归决策树模型====================
library(rpart)
sf.price.model.rpart <- rpart(
price~bedrooms+squarefeet+lotsize+latitude+
longitude+neighborhood+month,
data=sanfrancisco.home.sales.training)

#可视化决策树
plot(sf.price.model.rpart,uniform=TRUE,compress=TRUE,lty=3,branch=0.7)
text(sf.price.model.rpart,all=TRUE,digits=7,use.n=TRUE,cex=0.4,xpd=TRUE,)

library(maptree)
draw.tree(sf.price.model.rpart,cex=0.5,nodeinfo=TRUE,col=gray(0:8 / 8))

#检查决策树
rsq.rpart(sf.price.model.rpart)

prune(sf.price.model.rpart,cp=0.11)

plotcp(sf.price.model.rpart, minline = TRUE, lty = 3, col = 1,upper = c("size", "splits", "none"))

#输出决策树训练集和测试集误差
calculate_rms_error <- function(mdl, train, test, yval) {
train.yhat <- predict(object=mdl,newdata=train)
test.yhat <- predict(object=mdl,newdata=test)
train.y <- with(train,get(yval))
test.y <- with(test,get(yval))
train.err <- sqrt(mean((train.yhat - train.y)^2))
test.err <- sqrt(mean((test.yhat - test.y)^2))
c(train.err=train.err,test.err=test.err)
}

calculate_rms_error(sf.price.model.rpart,
sanfrancisco.home.sales.training,
sanfrancisco.home.sales.testing,
"price")

#==================bagging模型============================
library(ipred)

sf.price.model.bagging <- bagging(
price~bedrooms+squarefeet+lotsize+latitude+
longitude+neighborhood+month,
data=sanfrancisco.home.sales.training, nbagg=100)

summary(sf.price.model.bagging)

calculate_rms_error(sf.price.model.bagging,
sanfrancisco.home.sales.training,
sanfrancisco.home.sales.testing,
"price")

#===================boosting模型=========================
library(mboost)

sf.price.model.blackboost <- blackboost(
price~bedrooms+squarefeet+lotsize+latitude+
longitude+neighborhood+month,
data=sanfrancisco.home.sales.training)

summary(sf.price.model.blackboost)

calculate_rms_error(sf.price.model.blackboost,
sanfrancisco.home.sales.training,
sanfrancisco.home.sales.testing,
"price")

#====================RandomForest随机森林=========================
library(randomForest)

sf.price.model.randomforest <- randomForest(
price~bedrooms+squarefeet+lotsize+latitude+
longitude+month,
data=sanfrancisco.home.sales.training,
na.action=na.omit)

calculate_rms_error(sf.price.model.randomforest,
na.omit(sanfrancisco.home.sales.training),
na.omit(sanfrancisco.home.sales.testing),
"price")
