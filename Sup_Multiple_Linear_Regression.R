#读入棒球队数据
library(nutshell)
data(team.batting.00to08)
head(team.batting.00to08)

#绘制二维散点图
library(lattice)
#拆分多组自变量
attach(team.batting.00to08);
forplot = make.groups(
singles = data.frame(value=singles, teamID,yearID,runs),
doubles = data.frame(value=doubles, teamID,yearID,runs),
triples = data.frame(value=triples, teamID,yearID,runs),
homeruns = data.frame(value=homeruns, teamID,yearID,runs),
walks = data.frame(value=walks, teamID,yearID,runs),
stolenbases = data.frame(value=stolenbases, teamID,yearID,runs),
caughtstealing = data.frame(value=caughtstealing,teamID,yearID,runs),
hitbypitch = data.frame(value=hitbypitch, teamID,yearID,runs),
sacrificeflies = data.frame(value=sacrificeflies,teamID,yearID,runs)
);
#绘出二维图
xyplot(runs~value|which, data=forplot,scales=list(relation="free"),
pch=19, cex=.2,
strip=strip.custom(strip.levels=TRUE,
horizontal=TRUE,
par.strip.text=list(cex=.8))
)

#训练线性回归模型
runs.mdl <- lm(
formula=runs~singles+doubles+triples+homeruns+walks+hitbypitch+sacrificeflies+
stolenbases+caughtstealing,data=team.batting.00to08)

#查看训练完成模型的基本参数
view(runs.mdl)
formula(runs.mdl)
coef(runs.mdl)
summary(runs.mdl)
residuals(runs.mdl)
fitted(runs.mdl)

#使用训练完成的模型进行预测
predict(runs.mdl,team.batting.00to08)
