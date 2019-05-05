#读入案例数据
library(nutshell)
data(team.batting.00to08)
head(team.batting.00to08)

#建立主成分分析模型
batting.pca <- princomp(~singles+doubles+triples+homeruns
+walks+hitbypitch+sacrificeflies+stolenbases+caughtstealing,
data=team.batting.00to08)
batting.pca

#检查贡献率和负载矩阵
summary(batting.pca)
loadings(batting.pca)

#模型可视化
plot(batting.pca)
biplot(batting.pca,cex=0.5,col=c("gray50","black")
