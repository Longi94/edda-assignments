peruvians = read.csv(file = "peruvians.txt", header = TRUE, sep = " ")
peruvians = peruvians[, -c(5, 6, 7)]

# 1.
pairs(peruvians)

# gota think about it first

# 2.
attach(peruvians)
cor.test(age, migration, method = "spearman")
cor.test(weight, migration, method = "spearman")
cor.test(length, migration, method = "spearman")
cor.test(wrist, migration, method = "spearman")
cor.test(systolic, migration, method = "spearman")
cor.test(diastolic, migration, method = "spearman")

# As I understood, the higher the rho, the higher correlation ther is between the things, seems like age-migration has the highest correlation

# > cor.test(age,migration,method="spearman")
#
# Spearman's rank correlation rho
#
# data:  age and migration
# S = 5176.6, p-value = 0.002189
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho
# 0.4760575
#
# Warning message:
# In cor.test.default(age, migration, method = "spearman") :
# Cannot compute exact p-value with ties
# > cor.test(weight,migration,method="spearman")
#
# Spearman's rank correlation rho
#
# data:  weight and migration
# S = 6415.1, p-value = 0.02861
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho
# 0.3506956
#
# Warning message:
#   In cor.test.default(weight, migration, method = "spearman") :
#   Cannot compute exact p-value with ties
# > cor.test(length,migration,method="spearman")
#
# Spearman's rank correlation rho
#
# data:  length and migration
# S = 9044.3, p-value = 0.6087
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho
# 0.08458432
#
# Warning message:
# In cor.test.default(length, migration, method = "spearman") :
# Cannot compute exact p-value with ties
# > cor.test(wrist,migration,method="spearman")
#
# Spearman's rank correlation rho
#
# data:  wrist and migration
# S = 7712.8, p-value = 0.1797
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho
# 0.2193498
#
# Warning message:
#   In cor.test.default(wrist, migration, method = "spearman") :
#   Cannot compute exact p-value with ties
# > cor.test(systolic,migration,method="spearman")
#
# Spearman's rank correlation rho
#
# data:  systolic and migration
# S = 11544, p-value = 0.3054
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho
# -0.1684286
#
# Warning message:
# In cor.test.default(systolic, migration, method = "spearman") :
# Cannot compute exact p-value with ties
# > cor.test(diastolic,migration,method="spearman")
#
# Spearman's rank correlation rho
#
# data:  diastolic and migration
# S = 9137.6, p-value = 0.6494
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho
# 0.07514098
#
# Warning message:
#   In cor.test.default(diastolic, migration, method = "spearman") :
#   Cannot compute exact p-value with ties
