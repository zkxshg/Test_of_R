#读入射门数据
data(field.goals)
head(field.goals)

#创建射门成功二分变量
field.goals.forlr <- transform(field.goals,
good=as.factor(ifelse(play.type=="FG good","good","bad")))

#简单对比不同距离
field.goals.table <- table(field.goals.forlr$good,	field.goals.forlr$yards)field.goals.table

#绘出距离成功率散点图
plot(colnames(field.goals.table),
field.goals.table["good",]/
(field.goals.table["bad",] +
field.goals.table["good",]),
xlab="Distance (Yards)", ylab="Percent Good")

#训练逻辑回归模型
field.goals.mdl <- glm(formula=good~yards,data=field.goals.forlr,
family="binomial")
#查看模型参数
summary(field.goals.mdl)
 
#添加逻辑回归模型曲线
fg.prob <- function(y) {
eta <- 5.178856 + -0.097261 * y;
1 / (1 + exp(-eta))
}
lines(15:65,fg.prob(15:65),new=TRUE)
