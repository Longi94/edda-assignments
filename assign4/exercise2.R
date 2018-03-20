psidata = read.table("psi.txt", header = TRUE)
attach(psidata)

##########################
# 1.
##########################

# gpa distribution
hist(psidata[, 3], main = "gpa")

qqnorm(psidata[, 3])
qqline(psidata[, 3])

qqnorm(psidata[psi == 1, 3])
qqline(psidata[psi == 1, 3])

qqnorm(psidata[psi == 0, 3])
qqline(psidata[psi == 0, 3])

# bpxplots of gpa for pass/nopass and psi/nopsi
boxplot(psidata[passed == 0, 3], psidata[passed == 1, 3], names = c("Didn't pass", "Passed"))
boxplot(psidata[psi == 0, 3], psidata[psi == 1, 3], names = c("No PSI", "PSI"))

# table, number of individuals with all combinations of psi/passed
xtabs( ~ passed + psi, data = psidata)

# percentage of passed individuals for psi variable
round(xtabs(passed ~ psi, data = psidata) / xtabs( ~ passed, data = psidata), 2)

##########################
# 2.
##########################

psidata$psi = factor(psidata$psi)
psiglm = glm(passed ~ psi + gpa, data = psidata, family = binomial)
summary(psiglm)

##########################
# 3.
##########################

# coefficient of psi is positive (2.338), which means that using psi yielded better results.

##########################
# 4.
##########################

# this is probably not right, should be comupted from the summery of the model

psipassed = data.frame(psi = factor(1), gpa = 3)
predict(psiglm, psipassed, type = "response")

# 0.4815864

psinotpassed = data.frame(psi = factor(0), gpa = 3)
predict(psiglm, psinotpassed, type = "response")

# 0.08230274

# rather this ? (from the summary)
# psi
Ppsi = -11.602 + 2.338 + 3 * 3.063

#nopsi
Pnopsi = -11.602 + 3 * 3.063

##########################
# 5.
##########################

# code from internet, probably wrong, fuck me
# 10.35817 times more likely
exp(cbind(OR = coef(psiglm), confint(psiglm)))

##########################
# 6.
##########################

x = matrix(c(3, 15, 8, 6), 2, 2)
fisher.test(x)

# 15 and 6 are the number of people who did not show improvement. the null
# hypothesis is that the two factors (improved or not / psi or not) are
# independent from each other p-value is 0.0265, we reject the null hypothesis,
# there is a dependence

##########################
# 7.
##########################

# I think it's wrong because this kind of analysis ignores the gpa variable. And
# as we've seen it has a positive effect on passed

##########################
# 8.
##########################

# http://www.derangedphysiology.com/main/required-reading/statistics-and-interpretation-evidence/Chapter%201.5.2/qualitative-data-chi-square-test-and-fishers-exact-test
# Fisher's Exact Test is simpler, but i can only be used with that 2x2
# contingency table, better suited for small datasets, it is allos
# computationally intence. Logistic regression is better for continuous
# variables, but it assumes the independence of errors and no outliers

detach(psidata)
