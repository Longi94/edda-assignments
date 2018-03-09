data = read.table("bread.txt", header=TRUE)

# comment: looks like 2-way ANOVA?

#######################
# POINT 1
#######################
N = 3 #OR 18? /// #slices of bread
I = 3 #environment: cold, intermediate, warm
J = 2 #humidity: dry, wt


result = rbind(rep(1:I,each=N*J),rep(1:J,N*I),sample(1:(N*I*J)))
# UNFINISHED ????

#######################
# POINT 2
#######################
boxplot(hours~environment,data=data)
boxplot(hours~humidity,data=data)
attach(data)
interaction.plot(environment,humidity,hours)
interaction.plot(humidity,environment,hours)

#######################
# POINT 3
#######################
data$environment = as.factor(data$environment)
data$humidity = as.factor(data$humidity)
dataaov = lm(hours~environment*humidity, data=data)
anova(dataaov)
#comment: both factors have influence on results and interact with each other

contrasts(data$environment)=contr.sum
contrasts(data$humidity)=contr.sum
dataaov2=lm(hours~environment*humidity,data=data)
summary(dataaov2)
confint(dataaov2)
#######################
# POINT 4
#######################

#comment: We cannot judge which factor has the greatest influence on the decay since factors interact with each other.

#######################
# POINT 5
#######################
qqnorm(residuals(dataaov2))
qqline(residuals(dataaov2))
#extreme values are outliers

plot(fitted(dataaov2),residuals(dataaov2))
