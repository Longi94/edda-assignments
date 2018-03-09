library(multcomp)
library(lme4)

#tests should be correct, but I'm not really sure on what the results mean. Will read notes and try to analyze them.

data = read.table("cream.txt", header=TRUE)
attach(data)

data$starter = factor(data$starter)
data$batch = factor(data$batch)
data$position = factor(data$position)



#Exercise 1
threewaytest = aov(acidity~starter+batch+position, data=data)
summary(threewaytest)


#Exercise 2
dataaov = lm(acidity~starter+batch+position, data=data)
startermult = glht(dataaov,linfct=mcp(starter="Tukey"))
summary(startermult)
#still need to interpret results

#Exercise 3

#Exercise 4
confint(startermult, level=0.95)
#The intervals [1.3201,4.2999], [1.4701,4.4499], [2.3001,5.2799], and [2.3001,5.2799] do not contain the number zero.
