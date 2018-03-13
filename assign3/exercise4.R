library(multcomp)
library(lme4)

# load data
cows = read.table("cow.txt", header = TRUE)
attach(cows)

##########################
# 1.
##########################

# lecture 4, 1-way ANOVA
cowaov = lm(milk ~ treatment)
anova(cowaov)

# p-value 0.9675, null hypothesis not rejected (does not influence)

##########################
# 2.
##########################

# Dunno about this one
# same as 4.??
t.test(milk[treatment == "A"], milk[treatment == "B"], paired = TRUE)
# mean of differences is 0.2444444 

##########################
# 3.
##########################

# lecture 7, crossover design
cows$id = factor(id)
cows$per = factor(per)
cowslmer = lmer(milk ~ treatment + order + per + (1 | id),
                data = cows,
                REML = FALSE)
summary(cowslmer)

cowslmer1 = lmer(milk ~ order + per + (1 | id), data = cows, REML = FALSE)
anova(cowslmer1, cowslmer)
# p-value 0.446, accept the null hypothesis
# we got different results, why? idk

##########################
# 4.
##########################

t.test(milk[treatment == "A"], milk[treatment == "B"], paired = TRUE)
# this is an paired two sample t-test, which is a good choice since we know that the samples are paired
# t test also assumes, that the populations are normal, looking at the qq plots, this assumption is reasonable

qqnorm(milk[treatment == "A"])
qqline(milk[treatment == "A"])

qqnorm(milk[treatment == "B"])
qqline(milk[treatment == "B"])

# p-value 0.8281, so accept null hypo like in 1. I don't know why it is the same though