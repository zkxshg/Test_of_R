#导入数据
library(recommenderlab)
data(Jester5k)
head(Jester5k)
Jester5k

#查看特定评价
as(Jester5k[10,], "list")
rowMeans(Jester5k[10,])
colMeans(Jester5k[,1])

hist(getRatings(Jester5k), breaks=100)
hist(getRatings(normalize(Jester5k)), breaks = 100)

#数据预处理
set.seed(123)
e <- evaluationScheme(Jester5k, method="split",  train=0.8, given=15, goodRating=5)

#查看函数注册信息
recommenderRegistry$get_entries(dataType ="realRatingMatrix")

#基于用户的协同过滤模型
ubcf <- Recommender(getData(e,"train"), "UBCF")
user_pred <- predict(ubcf, getData(e, "known"), type = "ratings")
P1 <- calcPredictionAccuracy(user_pred, getData(e,"unknown"))

#基于项目的协同过滤模型
ibcf <- Recommender(getData(e,"train"), "IBCF")
item_pred <- predict(ibcf, getData(e, "known"), type = "ratings")
P2 <- calcPredictionAccuracy(item_pred, getData(e, "unknown"))
#对比模型的推荐效果
error <- rbind(P1, P2)
rownames(error) <- c("UBCF", "IBCF")
error
#测试具体用户推荐
R1 <- Recommender(Jester5k, method = "UBCF")
recommend <- predict(R1, Jester5k[1:2], n = 5)
as(recommend, "list")

#预测用户评价
rating <- predict(R1, Jester5k[300:309], type = "ratings")
rating
as(rating, "matrix")[,71:73]
