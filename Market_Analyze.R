#读入数据
library(arules)
library(arulesViz)
data(Groceries)
head(Groceries)

#查看事务型数据
str(Groceries)
itemFrequencyPlot(Groceries, topN = 10, type = "absolute")
# itemFrequencyPlot(Groceries, topN = 15)

#建立模型
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf =  0.9, maxlen=4))
rules 

#对模型进行排序和观察
options(digits = 2)
rules <- sort(rules, by = "lift", decreasing = TRUE)
inspect(rules[1:5])
rules <- sort(rules, by = "confidence", decreasing = TRUE)inspect(rules[1:5]

#建立关联表观察关联规则
tab <- crossTable(Groceries)
tab[1:6, 1:6]
tab["bottled beer","bottled beer"]
tab["bottled beer","canned beer"]

#生成特定商品关联规则
beer.rules <- apriori(data = Groceries, parameter = list(support = 0.0015, confidence = 0.3), appearance = list(default = "lhs",  rhs = "bottled beer"))beer.rules
beer.rules <- sort(beer.rules, decreasing = TRUE, by = "lift")
inspect(beer.rules)
#使用图表观察函数
tab["bottled beer", "red/blush wine"]
tab["red/blush wine", "red/blush wine"]
tab["white wine", "white wine"]
tab["bottled beer", "white wine"]

plot(beer.rules, method = "graph", measure = "lift", shading =  "confidence")
