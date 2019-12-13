# 讀入數據
Cow_report <- read.csv(file="report.csv", header=TRUE, sep=",",fileEncoding = "UTF-8-BOM")
# head(Cow_report)

# ==========================================================
# 初次刪除無關特征
# subset()篩除五項無關特征
Cow_report <- subset(Cow_report, select = -c(X6,X7,X15,X17,X21))

# ==========================================================
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

# ==========================================================
# 再次刪除變換後多餘特征
# subset()篩除多餘特征
Cow_report <- subset(Cow_report, select = -c(X2,X5,X8,X12,X13,X16,X19,X20))

# ==========================================================
# 基於X4農場進行分割
Cow_report_A <- Cow_report[Cow_report$X4=='A',]
Cow_report_B <- Cow_report[Cow_report$X4=='B',]
Cow_report_C <- Cow_report[Cow_report$X4=='C',]

# ==========================================================
# 劃分訓練集和測試集
# is.na()判斷空變量
Test_A <- Cow_report[is.na(Cow_report_A$X11),]
Test_B <- Cow_report[is.na(Cow_report_B$X11),]
Test_C <- Cow_report[is.na(Cow_report_C$X11),]

# complete.cases()判斷非空變量
Train_A <- Cow_report[complete.cases(Cow_report_A$X11),]
Train_B <- Cow_report[complete.cases(Cow_report_B$X11),]
Train_C <- Cow_report[complete.cases(Cow_report_C$X11),]

# ==========================================================
# 從外部讀入數據
# TestData
# Cow_Test_B <- read.csv(file="Test_B.csv", header=TRUE, sep=",")
# head(Cow_Test_B)

# TrainData
# Cow_Train_B <- read.csv(file="Train_B.csv", header=TRUE, sep=",")
# head(Cow_Train_B)

# ==========================================================
# =========================nnet模型預測=====================
Cow_Test_A <- Test_A
Cow_Train_A <- Test_A

# 建立神經網絡模型
library(nnet)
Cow_B.nnet <- nnet(
X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data=Cow_Train_A, size=9,
skip=TRUE, linout=TRUE, decay=0.025, na.action=na.omit)

# 預測測試集
Result_A <- predict(Cow_A.nnet,Cow_Test_A)

# 保存預測結果
write.csv(Result_A, file = "Result_A.csv")

# 檢查模型參數
summary(Cow_A.nnet)

# ==========================================================
# =========================多變量線性回歸模型預測===========
# 建立mdl模型
Cow_A.mdl <- lm(formula = X11~X3+X9+X10+X14+X18+A1+A2+A3+A4, data = Cow_Train_A)

# 預測訓練集
Result_A <- predict(Cow_A.mdl,Cow_Test_A)

# 保存結果
write.csv(Result_A, file = "Result_A.csv")

# 檢查回歸參數顯著性
formula(Cow_A.mdl)
coef(Cow_A.mdl)
summary(Cow_A.mdl)

# =============================================================
# Sub <- read.csv(file="submission.csv", header=TRUE, sep=",",fileEncoding = "UTF-8-BOM")
# Result <- left_join(Sub, Milk, by='ID')
