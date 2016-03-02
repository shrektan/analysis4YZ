
# ref http://blog.sina.com.cn/s/blog_6aed37fd0101qe27.html

library(dplyr)
library(ggplot2)
r <- readr::read_csv("data.csv", locale = readr::locale(encoding = "GB2312"), col_names = FALSE)
tmp <- purrr::flatten_chr(r)
tmp <- table(tmp)
tmp <- sort(tmp, decreasing = TRUE)
plot(tmp)



library(arules)

data(Groceries)

library(arulesViz)


# -----------------------------------------------------------------
  # apriori算法案例：简易应用之筛选购全脂牛奶 的关联分析情况

rules = apriori(Groceries,parameter = list(support = 0.01,confidence = 0.2))

inspect(sort(rules,by="support")[1:6]) #按支持度查看前6条规则
inspect(sort(rules,by="confidence")[1:6]) #按置信度查看前6条规则
sub.rules=subset(rules, subset = rhs %in% "whole milk" &lift > 1.2) #也可以用subset做规则的筛选,取"右手边"含有whole milk且lift大于1.2的规则

itemFrequencyPlot(Groceries,support = 0.05,cex.names =0.8) #数据画频繁项的图

plot(rules, shading="order", control=list(main = "Two-key plot"))#见chart.1
plot(rules, method="grouped")#见chart.2
plot(rules, method="graph")#见chart.3


# ----------------------------------------------------------------
  
  # eclat算法案例：简易应用之筛选购全脂牛奶 的关联分析情况


fsets <- eclat(Groceries, parameter = list(support = 0.05),control = list(verbose=FALSE)) #提取频繁项集

itemFrequencyPlot(Groceries,support = 0.05,cex.names =0.8) #数据画频繁项的图

itemsetList <- LIST(items(fsets), decode = FALSE)

singleItems <- fsets[size(items(fsets)) == 1] #筛选单独项集

singleSupport <- quality(singleItems)$support #单独项集的支持度

names(singleSupport) <- unlist(LIST(items(singleItems), decode = FALSE)) #转换为列表格式的数据

allConfidence <- quality(fsets)$support / sapply(itemsetList, function(x)max(singleSupport[as.character(x)]))

quality(fsets) <- cbind(quality(fsets), allConfidence)

fsetsmilk <- subset(fsets, subset = items %pin% "whole milk")

inspect(sort(fsetsmilk[size(fsetsmilk)>1], by = "allConfidence")[1:3])

