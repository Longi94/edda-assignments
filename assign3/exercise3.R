library(multcomp)
library(lme4)

#tests should be correct, but I'm not really sure on what the results mean. Will read notes and try to analyze them.

data = read.table("cream.txt", header=TRUE)
data$starter = factor(data$starter)
data$batch = factor(data$batch)
data$position = factor(data$position)
attach(data)

#Exercise 1
model1 = lm(acidity~starter+batch+position,data)
model2 = update(model1, . ~ . - starter:batch:position) #remove interactions
data.aov = aov(model2,data)
summary(data.aov)

#Exercise 2
model = lm(acidity~starter+batch+position, data=data)
startermult = glht(model,linfct=mcp(starter="Tukey"))
summary(startermult)


#Exercise 3


#Exercise 4
confint(startermult, level=0.95)
#The intervals [1.3201,4.2999], [1.4701,4.4499], [2.3001,5.2799], and [-4.7835,-1.8045] do not contain the number zero.
