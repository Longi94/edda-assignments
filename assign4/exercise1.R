data = read.table("fruitflies.txt", header=TRUE)

###############
## POINT 1 
###############
loglongevity = c(1:length(data$longevity))

for (i in 1:length(data$longevity)) {
  loglongevity[i] = log(data$longevity[i])
}

data = cbind(data,loglongevity)


###############
## POINT 2 
###############
boxplot(loglongevity~activity,data=data)

#what else?


###############
## POINT 3
###############

#statistical test 1-way ANOVA
# numerical output: loglongevity, fixed factor levels: activity
dataaov=lm(loglongevity~activity,data=data)
anova(dataaov)
# p < 0.05 we should reject H0, activity has significant impact on longevity

###############
## POINT 4
###############
#estimations:
summary(dataaov)
# u1 = 3.60 - high activity
# u2 - u1 = 0.52 => u2 = 4.12 - isolated activity
# u3 - u1 = 0.39 => u3 = 3.99 - low activity

#we can say that higher activity descreases longevity
confint(dataaov)

###############
## POINT 5
###############
# ANCOVA:
# numerical output - loglongivity
# fixed factor: activity
# numerical explanatory variable: thorax


# is it useful?

data$activity=as.factor(data$activity)
dataaov2=lm(loglongevity~thorax+activity,data=data)  # type second!!!
anova(dataaov2)
# p are < 0.05 - both factors have significant influence on outcome

drop1(dataaov2, test="F")


###############
## POINT 6
###############
contrasts(data$activity)=contr.sum
summary(dataaov2)
confint(dataaov2)

###############
## POINT 7
###############
plot(loglongevity~thorax,pch=as.character(activity))

###############
## POINT 8
###############
#without thorax, thorax is dependent from logitivity, not opposite

###############
## POINT 9
###############
qqnorm(residuals(dataaov2))
plot(fitted(dataaov2),residuals(dataaov2))
#normality confirmed

###############
## POINT 10
###############
data$activity=as.factor(data$activity)
dataaov3=lm(longevity~thorax+activity,data=data)  # type second!!!
anova(dataaov3)
qqnorm(residuals(dataaov3))
plot(fitted(dataaov3),residuals(dataaov3))
