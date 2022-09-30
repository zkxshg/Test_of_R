# Data:
# https://www.kaggle.com/ashishvaya/recommendation-engine
# Package:
# https://www.rdocumentation.org/packages/recommenderlab/versions/0.2-5
# Publish:
# [1]	周成 and 王可可, "在线信息学竞赛训练的组合推荐机制研究," 福建电脑, vol. 37, no. 06, pp. 135-140, 2021.

# POJ:
# https://zh.wikipedia.org/wiki/%E5%9C%A8%E7%BA%BF%E8%AF%84%E6%B5%8B%E7%B3%BB%E7%BB%9F#cite_note-6

# According:
# Skiena, S., & Revilla, M. (2003). Programming challenges. Springer-Verlag.
# Francisco, R. E. and A. P. Ambrosio (2015). Mining an Online Judge System to Support Introductory Computer Programming Teaching. EDM (Workshops).
# Michael Hahsler (2016). recommenderlab: A Framework for Developing and Testing Recommendation Algorithms, R package.
# Koren, Y., et al. (2009). "Matrix factorization techniques for recommender systems." Computer(8): 30-37.
# Yera Toledo, R., et al. (2018). A recommender system for programming online judges using fuzzy information modeling. 
# + Informatics, Multidisciplinary Digital Publishing Institute.

library(recommenderlab)
library(ggplot2)

# 数据讀入
Sub <- read.csv(file="recommendation-engine/train_submissions.csv", header=TRUE, sep=",") 

# 轉換為评分矩阵
data_matrix <- matrix(0,3571,6544)
for (line in 1:nrow(Sub)){
	data_matrix[
	as.integer(substr(as.character(Sub[line,1]),6,nchar(as.character(Sub[line,1])))),
 	as.integer(substr(as.character(Sub[line,2]),6,nchar(as.character(Sub[line,2]))))] <-
	as.integer(Sub[line,3])}

# 清除空行	
data_matrix_full <- data_matrix
null_index <-  c()
for (line in 1:nrow(data_matrix_full)){
	if(sum(data_matrix_full[line,])==0){null_index <- append(null_index, line)} 
	}
data_matrix_full <- data_matrix_full[-null_index,]

# 矩陣->稀疏矩陣->評分矩陣
POJ_dgCM <- as(data_matrix_full,"dgCMatrix")
POJ_RM <- new("realRatingMatrix", data=POJ_dgCM)

# Summary
as(POJ_RM[10,], "list")
rowMeans(POJ_RM[10,])
colMeans(POJ_RM[,1])	
rownames(POJ_RM) <- paste("U", 1:nrow(data_matrix_full), sep = "")
colnames(POJ_RM) <- paste("M", 1:6544, sep = "")
hist(getRatings(POJ_RM), breaks=6)

# 预处理
set.seed(123)
POJ_RM_e <- evaluationScheme(POJ_RM, method="split",  train=0.8, given=1, goodRating=2)

# ===========================================1 最优参数============================================

#==================================1-1 UBCF===========================================
# =================================建立模型====================================
# ==================余弦距离===========
# 未正规化
UBCF_N_C <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = NULL, method="Cosine"))
# 中心化
UBCF_C_C <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = "center",method="Cosine"))
# z分数标准化
UBCF_Z_C <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = "Z-score",method="Cosine"))
	  
# ==================欧氏距离===========
# 未正规化
UBCF_N_E <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = NULL, method="Euclidean"))
# 中心化
UBCF_C_E <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = "center",method="Euclidean"))
# z分数标准化
UBCF_Z_E <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = "Z-score",method="Euclidean"))
# ==================皮尔逊相关系数===========
# 未正规化
UBCF_N_P <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = NULL, method="pearson"))
# 中心化
UBCF_C_P <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = "center",method="pearson"))
# z分数标准化
UBCF_Z_P <- Recommender(getData(POJ_RM_e, "train"), "UBCF", 
      param=list(normalize = "Z-score",method="pearson"))

# =================================进行预测====================================
p1 <- predict(UBCF_N_C, getData(POJ_RM_e, "known"), type="ratings")
p2 <- predict(UBCF_C_C, getData(POJ_RM_e, "known"), type="ratings")
p3 <- predict(UBCF_Z_C, getData(POJ_RM_e, "known"), type="ratings")

p4 <- predict(UBCF_N_E, getData(POJ_RM_e, "known"), type="ratings")
p5 <- predict(UBCF_C_E, getData(POJ_RM_e, "known"), type="ratings")
p6 <- predict(UBCF_Z_E, getData(POJ_RM_e, "known"), type="ratings")

p7 <- predict(UBCF_N_P, getData(POJ_RM_e, "known"), type="ratings")
p8 <- predict(UBCF_C_P, getData(POJ_RM_e, "known"), type="ratings")
p9 <- predict(UBCF_Z_P, getData(POJ_RM_e, "known"), type="ratings")

# =================================计算误差====================================  
P1 <- calcPredictionAccuracy(p1, getData(POJ_RM_e,"unknown"))
P2 <- calcPredictionAccuracy(p2, getData(POJ_RM_e,"unknown"))
P3 <- calcPredictionAccuracy(p3, getData(POJ_RM_e,"unknown"))
P4 <- calcPredictionAccuracy(p4, getData(POJ_RM_e,"unknown"))
P5 <- calcPredictionAccuracy(p5, getData(POJ_RM_e,"unknown"))
P6 <- calcPredictionAccuracy(p6, getData(POJ_RM_e,"unknown"))
P7 <- calcPredictionAccuracy(p7, getData(POJ_RM_e,"unknown"))
P8 <- calcPredictionAccuracy(p8, getData(POJ_RM_e,"unknown"))
P9 <- calcPredictionAccuracy(p9, getData(POJ_RM_e,"unknown"))

#==================================1-2 IBCF===========================================
# =================================建立模型====================================
# ==================余弦距离===========
# 未正规化
IBCF_N_C <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = NULL, method="Cosine"))
# 中心化
IBCF_C_C <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = "center",method="Cosine"))
# z分数标准化
IBCF_Z_C <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = "Z-score",method="Cosine"))
	  
# ==================欧氏距离===========
# 未正规化
IBCF_N_E <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = NULL, method="Euclidean"))
# 中心化
IBCF_C_E <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = "center",method="Euclidean"))
# z分数标准化
IBCF_Z_E <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = "Z-score",method="Euclidean"))
# ==================皮尔逊相关系数===========
# 未正规化
IBCF_N_P <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = NULL, method="pearson"))
# 中心化
IBCF_C_P <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = "center",method="pearson"))
# z分数标准化
IBCF_Z_P <- Recommender(getData(POJ_RM_e, "train"), "IBCF", 
      param=list(normalize = "Z-score",method="pearson"))

# =================================进行预测====================================
p21 <- predict(IBCF_N_C, getData(POJ_RM_e, "known"), type="ratings")
p22 <- predict(IBCF_C_C, getData(POJ_RM_e, "known"), type="ratings")
p23 <- predict(IBCF_Z_C, getData(POJ_RM_e, "known"), type="ratings")

p24 <- predict(IBCF_N_E, getData(POJ_RM_e, "known"), type="ratings")
p25 <- predict(IBCF_C_E, getData(POJ_RM_e, "known"), type="ratings")
p26 <- predict(IBCF_Z_E, getData(POJ_RM_e, "known"), type="ratings")

p27 <- predict(IBCF_N_P, getData(POJ_RM_e, "known"), type="ratings")
p28 <- predict(IBCF_C_P, getData(POJ_RM_e, "known"), type="ratings")
p29 <- predict(IBCF_Z_P, getData(POJ_RM_e, "known"), type="ratings")

# =================================计算误差====================================  
P_21 <- calcPredictionAccuracy(p21, getData(POJ_RM_e,"unknown"))
P_22 <- calcPredictionAccuracy(p22, getData(POJ_RM_e,"unknown"))
P_23 <- calcPredictionAccuracy(p23, getData(POJ_RM_e,"unknown"))
P_24 <- calcPredictionAccuracy(p24, getData(POJ_RM_e,"unknown"))
P_25 <- calcPredictionAccuracy(p25, getData(POJ_RM_e,"unknown"))
P_26 <- calcPredictionAccuracy(p26, getData(POJ_RM_e,"unknown"))
P_27 <- calcPredictionAccuracy(p27, getData(POJ_RM_e,"unknown"))
P_28 <- calcPredictionAccuracy(p28, getData(POJ_RM_e,"unknown"))
P_29 <- calcPredictionAccuracy(p29, getData(POJ_RM_e,"unknown"))

# ===========================================2 组合模型权重============================================
# 測試組合模型
HybridR <- HybridRecommender(
  UBCF_Z_P,
  IBCF_N_E,
  pop,
  weights = c(.6, .1, .3)
  )
Hybrid_pre <- predict(HybridR, getData(POJ_RM_e, "known"), type="ratings")
Hybrid_Acc <- calcPredictionAccuracy(Hybrid_pre, getData(POJ_RM_e,"unknown"))
# 定義組合函數
# 基於传入权重建立模型
HybRecomF<-function(HybWeight){
	HybridR <- HybridRecommender(
		UBCF_Z_P,
		IBCF_N_E,
		pop,
		weights = HybWeight
		)
	Hybrid_pre <- predict(HybridR, getData(POJ_RM_e, "known"), type="ratings")
	Hybrid_Acc <- calcPredictionAccuracy(Hybrid_pre, getData(POJ_RM_e,"unknown"))
	return(Hybrid_Acc)
}
# 測試最優權重
# 固定流行度權重
Test_Hyb_Acc <- c()
Test_RMSE <- c()
Test_MSE <- c()
Test_MAE <- c()
for (num in 0:12){
	weight <- c(0.1+num*0.05, 0.7-num*0.05, 0.2)
	Test_Hyb_Acc <- HybRecomF(weight)
	Test_RMSE <- append(Test_RMSE, as.numeric(Test_Hyb_Acc)[1])
	Test_MSE <- append(Test_MSE, as.numeric(Test_Hyb_Acc)[2])
	Test_MAE <- append(Test_MAE, as.numeric(Test_Hyb_Acc)[3])
	}
# 繪圖比較
Ratio <- c()
for (i in 0:12){Ratio <- append(Ratio, 0.1+i*0.05)}
plot_data <- data.frame(Ratio, Test_RMSE, Test_MSE, Test_MAE)
p1<-ggplot(plot_data, aes(x=Ratio)) + 
            geom_point(aes(y=Test_RMSE), ) + 
            geom_line(aes(y=Test_RMSE, , color="cyan")) +
            geom_point(aes(y=Test_MSE)) + 
            geom_line(aes(y=Test_MSE, color="red")) +
			geom_point(aes(y=Test_MAE)) + 
            geom_line(aes(y=Test_MAE, color="green"))
  
# 基礎协同过滤
ubcf <- Recommender(getData(POJ_RM_e,"train"), "UBCF")
ibcf <- Recommender(getData(POJ_RM_e,"train"), "IBCF")
pop <- Recommender(getData(POJ_RM_e,"train"), "POPULAR")
rand <- Recommender(getData(POJ_RM_e,"train"), "RANDOM")
# 分别进行预测
user_pred_ub <- predict(ubcf, getData(POJ_RM_e, "known"), type = "ratings")
user_pred_ib <- predict(ibcf, getData(POJ_RM_e, "known"), type = "ratings")
user_pred_pop <- predict(pop, getData(POJ_RM_e, "known"), type = "ratings")
user_pred_rand <- predict(rand, getData(POJ_RM_e, "known"), type = "ratings")
P1 <- calcPredictionAccuracy(user_pred, getData(POJ_RM_e,"unknown"))

# 预测具体用户推荐
recommend <- predict(ubcf, POJ_RM[1:2], n = 5)
as(recommend, "list")

# 预测用户具体评价
rating <- predict(ubcf, POJ_RM[300:309], type = "ratings")
as(rating, "matrix")[,71:73]
