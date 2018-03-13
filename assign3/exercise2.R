library(multcomp)
library(lme4)

# read the data
search = read.table("search.txt", header = TRUE)
attach(search)

##########################
# 1.
##########################

# 3 types of interfaces, 5 types of studends (3 each)
# We make a randomized block design (see lecture 6)

B = 5 # 5 user skill levels
I = 3 # 3 interface types

# number the students in skill order ({1,2,3} is skill 1, {4,5,6} is skill 2, etc.)

blocks = matrix(0, B, I)

for (i in 1:B) {
  blocks[i, ] = sample(((i - 1) * I + 1):(i * I))
}

# in each row, first student goes to interface 1, second to 2 and third to 3
blocks

##########################
# 2.
##########################

# again, randomized block design in lecture 6

# show data in table
# xtabs(time ~ interface + skill, data = search)

par(mfrow = c(1, 2))

# boxplots
boxplot(time ~ interface)
boxplot(time ~ skill)

# interaction plots there is a significant interaction between skill and interface
# http://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/anova/how-to/interaction-plot/interpret-the-results/
interaction.plot(interface, skill, time)
interaction.plot(skill, interface, time)

##########################
# 3.
##########################

# we do a 2-way ANOVA (lecture 5)
# skill has no interaction with search interfaces so we use the additive test
search$interface = as.factor(interface)
search$skill = as.factor(skill)

aovser = lm(time ~ interface + skill, data = search)
anova(aovser)

# the null hypothetsis can be rejected, result:
#
# Analysis of Variance Table
#
# Response: time
# Df Sum Sq Mean Sq F value  Pr(>F)
# as.factor(interface)  2 50.465 25.2327  7.8237 0.01310 *
#   as.factor(skill)      4 80.051 20.0127  6.2052 0.01421 *
#   Residuals             8 25.801  3.2252
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##########################
# 4.
##########################

summary(aovser)

# multiple comparisons (lecture 6, slide 6)
multser = glht(aovser, linfct = mcp(skill = "Tukey"))
summary(multser)
# estimate for skill level 4 and interface 3 (4-3) is 2.267

# wait I think this is the standard deviation, all the results are around 20, im not sure about this one

##########################
# 5.
##########################

# lecture 5, slides 26-27, lecture 6 slide 18
qqnorm(residuals(aovser))
plot(fitted(aovser), residuals(aovser))

# the plots look OK (just like how the slides say :) ), normal dist on QQ, even spread on the second

##########################
# 6.
##########################

# Lecture 6, Friedman
friedman.test(time, interface, skill)
# p-value = 0.04076, we reject the null hypothesis, there is an effect

##########################
# 7.
##########################

# Lecture 4, 1-way ANOVA
aovinter = lm(time ~ interface, data = search)
anova(aovinter)
# p-value = 0.09642, we accept the null hypothesis that the search is the same for all interfaces

# right/wrong??? useful or not????
# since each interface is tested with the same kind of students, this would indicate that the interfaces
# actually do have the same time, so I guess this is useful?

# q-way ANOVA assumes, that the data comes from normal distributions, the plots imply normal distributions so this condition is met.
qqnorm(residuals(aovinter))
qqline(residuals(aovinter))

detach(search)
