library(dplyr)
library(zoo)
library(fpp2)

# 讀入數據
Cow_report <- read.csv(file="aggregate/report.csv", header=TRUE, sep=",",fileEncoding = "UTF-8-BOM")
# head(Cow_report)
Sub <- read.csv(file="aggregate/submission.csv", header=TRUE, sep=",",fileEncoding = "UTF-8-BOM")
# ====================================================================================
# 初次刪除無關特征: subset()篩除五項無關特征
Cow_report <- subset(Cow_report, select = -c(X6,X7,X15,X17,X21))
# ====================================================================================
# 處理時間變量
# 處理缺失值
Cow_report$X19[Cow_report$X19==''] <- 0
# 變換日期格式
Cow_report$X8 <-as.Date(Cow_report$X8)
Cow_report$X12 <-as.Date(Cow_report$X12)
Cow_report$X13 <-as.Date(Cow_report$X13)
Cow_report$X16 <-as.Date(Cow_report$X16)
Cow_report$X19 <-as.Date(Cow_report$X19)
Cow_report$X20 <-as.Date(Cow_report$X20)
# 進行差分計算
# 第一次配種周齡
Cow_report$A1 <- as.integer(difftime(Cow_report$X20, Cow_report$X8, units = "weeks"))
# 最近分娩周齡
Cow_report$A2 <- as.integer(difftime(Cow_report$X13, Cow_report$X12, units = "weeks"))
# 最後配種周齡
Cow_report$A3 <- as.integer(difftime(Cow_report$X16, Cow_report$X8, units = "weeks"))
# 上次分娩月齡
Cow_report$A4 <- as.integer(difftime(Cow_report$X19, Cow_report$X8, units = "weeks"))
# ====================================================================================
# 再次刪除變換後多餘特征: subset()篩除多餘特征
Cow_report <- subset(Cow_report, select = -c(X2,X5,X8,X12,X13,X16,X19,X20))
# ====================================================================================
# 基於X4農場進行分割
Cow_report_A <- Cow_report[Cow_report$X4=='A',]
Cow_report_B <- Cow_report[Cow_report$X4=='B',]
Cow_report_C <- Cow_report[Cow_report$X4=='C',]
# ====================================================================================
# 劃分訓練集和測試集
# is.na()判斷空變量
Test_A <- Cow_report_A[is.na(Cow_report_A$X11),]
Test_B <- Cow_report_B[is.na(Cow_report_B$X11),]
Test_C <- Cow_report_C[is.na(Cow_report_C$X11),]

# complete.cases()判斷非空變量
Train_A <- Cow_report_A[complete.cases(Cow_report_A$X11),]
Train_B <- Cow_report_B[complete.cases(Cow_report_B$X11),]
Train_C <- Cow_report_C[complete.cases(Cow_report_C$X11),]

# =================== 處理缺失值 =======================================
# ======================= 方法1：填補測試集缺失值 =======================
Test_A$A1 <- na.interp(Test_A$A1)
Test_A$A2 <- na.interp(Test_A$A2)
Test_A$A3 <- na.interp(Test_A$A3)
Test_A$A4 <- na.interp(Test_A$A4)
Test_B$A1 <- na.interp(Test_B$A1)
Test_B$A2 <- na.interp(Test_B$A2)
Test_B$A3 <- na.interp(Test_B$A3)
Test_B$A4 <- na.interp(Test_B$A4)
Test_C$A1 <- na.interp(Test_C$A1)
Test_C$A2 <- na.interp(Test_C$A2)
Test_C$A3 <- na.interp(Test_C$A3)
Test_C$A4 <- na.interp(Test_C$A4)


# ======================= 方法2：用零替換缺失值 =========================

Test_A$A1[is.na(Test_A$A1)] <- 0 
Test_A$A2[is.na(Test_A$A2)] <- 0 
Test_A$A3[is.na(Test_A$A3)] <- 0 
Test_A$A4[is.na(Test_A$A4)] <- 0 
Test_B$A1[is.na(Test_B$A1)] <- 0 
Test_B$A2[is.na(Test_B$A2)] <- 0 
Test_B$A3[is.na(Test_B$A3)] <- 0 
Test_B$A4[is.na(Test_B$A4)] <- 0 
Test_C$A1[is.na(Test_C$A1)] <- 0 
Test_C$A2[is.na(Test_C$A2)] <- 0 
Test_C$A3[is.na(Test_C$A3)] <- 0 
Test_C$A4[is.na(Test_C$A4)] <- 0 

# ================================================================
Cow_Test_A <- Test_A
Cow_Train_A <- Train_A
Cow_Test_B <- Test_B
Cow_Train_B <- Train_B
Cow_Test_C <- Test_C
Cow_Train_C <- Train_C
# =========================================================================
# =========================模型1：多變量線性回歸模型預測======================
# 建立mdl模型
Cow_A.mdl <- lm(formula = X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data = Cow_Train_A)
# 預測訓練集
Result_A <- subset(Cow_Test_A, select = c(X1,X11))
colnames(Result_A) <- c('ID', 'x1')
Result_A$x1 <- predict(Cow_A.mdl,Cow_Test_A)

# 建立Cow_B.mdl模型
Cow_B.mdl <- lm(formula = X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data = Cow_Train_B, na.action=na.exclude)
Result_B <- subset(Cow_Test_B, select = c(X1,X11))
colnames(Result_B) <- c('ID', 'x1')
Result_B$x1 <- predict(Cow_B.mdl,Cow_Test_B)

# 建立Cow_C.mdl模型
Cow_C.mdl <- lm(formula = X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data = Cow_Train_C, na.action=na.exclude)
Result_C <- subset(Cow_Test_C, select = c(X1,X11))
colnames(Result_C) <- c('ID', 'x1')
Result_C$x1 <- predict(Cow_C.mdl,Cow_Test_C)

# 合併模型結果
Result_Total <- rbind(Result_A, Result_B, Result_C)
Result <- left_join(Sub, Result_Total, by='ID')
# 保存結果
write.csv(Result, file = "aggregate/Result_mdl.csv")

# =========================================================================
# =========================模型2：NNAR模型預測==============================

# 建立神經網絡模型
library(nnet)
Cow_A.nnet <- nnet(
X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data=Cow_Train_A, size=9,
skip=TRUE, linout=TRUE, decay=0.025, na.action=na.exclude)

Cow_B.nnet <- nnet(
X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data=Cow_Train_B, size=9,
skip=TRUE, linout=TRUE, decay=0.025, na.action=na.exclude)

Cow_C.nnet <- nnet(
X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data=Cow_Train_C, size=9,
skip=TRUE, linout=TRUE, decay=0.025, na.action=na.exclude)

# 預測測試集
Result_A <- subset(Cow_Test_A, select = c(X1,X11))
colnames(Result_A) <- c('ID', 'x1')
Result_A$x1 <- predict(Cow_A.nnet,Cow_Test_A)

Result_B <- subset(Cow_Test_B, select = c(X1,X11))
colnames(Result_B) <- c('ID', 'x1')
Result_B$x1 <- predict(Cow_B.nnet,Cow_Test_B)

Result_C <- subset(Cow_Test_C, select = c(X1,X11))
colnames(Result_C) <- c('ID', 'x1')
Result_C$x1 <- predict(Cow_C.nnet,Cow_Test_C)

# 合併模型結果
Result_Total <- rbind(Result_A, Result_B, Result_C)
Result <- left_join(Sub, Result_Total, by='ID')
# 保存結果
write.csv(Result, file = "aggregate/Result_NN.csv")
# =========================================================================
# ================================xgboost模型預測===========================
# 預處理數據滿足xgboost要求
Cow_Test_A <- subset(Cow_Test_A, select = -c(X1,X4,X11))
Cow_Test_B <- subset(Cow_Test_B, select = -c(X1,X4,X11))
Cow_Test_C <- subset(Cow_Test_C, select = -c(X1,X4,X11))

Cow_Train_A_y <- subset(Cow_Train_A, select = X11)
Cow_Train_B_y <- subset(Cow_Train_B, select = X11)
Cow_Train_C_y <- subset(Cow_Train_C, select = X11)
Cow_Train_A <- subset(Cow_Train_A, select = -c(X1,X4,X11))
Cow_Train_B <- subset(Cow_Train_B, select = -c(X1,X4,X11))
Cow_Train_C <- subset(Cow_Train_C, select = -c(X1,X4,X11))

# 建立XGboost模型
# max_depth = 35 || subsample = .5 || min_child_weight = 4
xgb_A <- xgboost(data = data.matrix(Cow_Train_A), 
 label = Cow_Train_A_y$X11, 
 max_depth = 35,
 min_child_weight = 4,
 nround=10000, 
 objective = "reg:linear",
 eval_metric = "rmse",
 subsample = .5,
 colsample_bytree = .8
 )
# max_depth = 15 || subsample = .8 || min_child_weight = 2
 xgb_B <- xgboost(data = data.matrix(Cow_Train_B), 
 label = Cow_Train_B_y$X11, 
 max_depth = 15,
 min_child_weight = 2,
 nround=5000, 
 objective = "reg:linear",
 eval_metric = "rmse",
 subsample = .8,
 colsample_bytree = .8
 )
# max_depth = 30 || subsample = .5 || min_child_weight = 2
 xgb_C <- xgboost(data = data.matrix(Cow_Train_C), 
 label = Cow_Train_C_y$X11, 
 max_depth = 30,
 min_child_weight = 2,
 nround=10000, 
 objective = "reg:linear",
 eval_metric = "rmse",
 subsample = .8,
 colsample_bytree = .8
 )
# 預測測試集
Result_A <- subset(Test_A, select = c(X1,X11))
colnames(Result_A) <- c('ID', 'x1')
Result_A$x1 <- predict(xgb_A,data.matrix(Cow_Test_A))

Result_B <- subset(Test_B, select = c(X1,X11))
colnames(Result_B) <- c('ID', 'x1')
Result_B$x1 <- predict(xgb_B,data.matrix(Cow_Test_B))

Result_C <- subset(Test_C, select = c(X1,X11))
colnames(Result_C) <- c('ID', 'x1')
Result_C$x1 <- predict(xgb_C,data.matrix(Cow_Test_C))

# 合併模型結果
Result_Total <- rbind(Result_A, Result_B, Result_C)
Result <- left_join(Sub, Result_Total, by='ID')
# 保存結果
write.csv(Result, file = "aggregate/Result_Boost.csv")
