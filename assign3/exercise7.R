library(multcomp)
library(lme4)

expensescrime = read.csv(file = "expensescrime.txt", header = TRUE, sep = " ")
attach(expensescrime)

# this whole thing is a multiple linear regression (lecture 8 and lacture 9)
pairs(expensescrime[,c(2, 5, 6)], panel=panel.smooth)

# step up method
summary(lm(expend ~ bad, data = expensescrime))
# R-squared 0.6964

summary(lm(expend ~ crime, data = expensescrime))
# R-squared 0.1119

summary(lm(expend ~ lawyers, data = expensescrime))
# R-squared 0.9373

summary(lm(expend ~ employ, data = expensescrime))
# R-squared 0.954

summary(lm(expend ~ pop, data = expensescrime))
# R-squared 0.9073

# employ has the highest value

summary(lm(expend ~ employ + bad, data = expensescrime))
# R-squared 0.9551

summary(lm(expend ~ employ + crime, data = expensescrime))
# R-squared 0.9551

summary(lm(expend ~ employ + lawyers, data = expensescrime))
# R-squared 0.9632

summary(lm(expend ~ employ + pop, data = expensescrime))
# R-squared 0.9543

# lawyers has the highest value and still yields significant explanatory variables

summary(lm(expend ~ employ + lawyers + bad, data = expensescrime))
# R-squared 0.9639

summary(lm(expend ~ employ + lawyers + crime, data = expensescrime))
# R-squared 0.9632

summary(lm(expend ~ employ + lawyers + pop, data = expensescrime))
# R-squared 0.9637

# all of these yield insignificant explanatory variables

summary(lm(expend ~ employ + lawyers, data = expensescrime))
# expend = -1.107e+02 + 2.686e-02 * lawyers + 2.971e-02 * employ + error

# step down

summary(lm(expend ~ employ + lawyers + bad + crime + pop, data = expensescrime))
# remove crime with p-value 0.25534

summary(lm(expend ~ employ + lawyers + bad + pop, data = expensescrime))
# remove pop with p-value 0.06012

summary(lm(expend ~ employ + lawyers + bad, data = expensescrime))
# remove bad with p-value 0.34496

summary(lm(expend ~ employ + lawyers, data = expensescrime))
# same model as step up


expendlm = lm(expend ~ employ + lawyers, data = expensescrime)

# influence point 
round(cooks.distance(expendlm), 2)
# there is no influence point, none of the values are above 1

# collinearity
# on the scattor plots employ and lawyers look very collinear
round(cor(expensescrime[,2:7]), 2)
# this also shows a very high collinearity between the two (0.97)
# having emply only makes more sense I guess?


# Residuals
qqnorm(residuals(expendlm))
qqline(residuals(expendlm))
# doesn't look normal, uuuuh what else?

detach(expensescrime)
