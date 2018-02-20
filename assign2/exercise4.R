clouds = read.table("clouds.txt",header=TRUE)
par(mfrow=c(2,2))
boxplot(clouds[,1],clouds[,2],names=c("seeded","unseeded"))
plot(clouds[,1],clouds[,2])
abline(0,1)

#TESTING CORRELATION
cor.test(clouds[,1],clouds[,2],method="spearman")
#rho is low (0,168) - no signifficant correlation

seeded = clouds[,1]
unseeded = clouds[,2]

#######################
# POINT 1
#######################

# TWO SAMLES T-TEST
hist(seeded)
hist(unseeded)
qqnorm(seeded)
qqnorm(unseeded)
#t.test should be used for nornal distribution, our is random
t.test(seeded,unseeded)
# p = 0.05375
#H0 accepted - seeded clouds have different mean than unseeded, but they are not from normal distribution, we cannot trust this test

# MANN-WHITNEY TEST
# wilcox test is appropriate for random idstribution - it compares two means
wilcox.test(seeded,unseeded)
# p=0.013 - we reject H0 that populations are equal

# KOLMOGOROV-SMIRNOV
ks.test(seeded,unseeded)
# p=0.019 - H0 rejected

#######################
# POINT 2
#######################
sqrt_seeded = sqrt(seeded)
sqrt_unseeded = sqrt(unseeded)

# TWO SAMLES T-TEST
hist(sqrt_seeded)
hist(sqrt_unseeded)
qqnorm(sqrt_seeded)
qqnorm(sqrt_unseeded)
#t.test should be used for nornal distribution, our is random
t.test(sqrt_seeded,sqrt_unseeded)
# p = 0.01956
# performing t-test on square roots, now histograms and qqplots ale closer to normal distribution, t.test works fine

# MANN-WHITNEY TEST
# wilcox test is appropriate for random idstribution - it compares two means
wilcox.test(sqrt_seeded,sqrt_unseeded)
# p=0.013 - we reject H0 that populations are equal - wilcox test didn't change

# KOLMOGOROV-SMIRNOV
ks.test(sqrt_seeded,sqrt_unseeded)
# p=0.019 - H0 rejected - ks test didn't change

#######################
# POINT 3
#######################
sqrt2_seeded = sqrt(sqrt(seeded))
sqrt2_unseeded = sqrt(sqrt(unseeded))

# TWO SAMLES T-TEST
hist(sqrt2_seeded)
hist(sqrt2_unseeded)
qqnorm(sqrt2_seeded)
qqnorm(sqrt2_unseeded)
#t.test should be used for nornal distribution, our is random
t.test(sqrt2_seeded,sqrt2_unseeded)
# p = 0.01256
# performing t-test on square2 roots, now histograms and qqplots resamble normal distribution, t.test works even better

# MANN-WHITNEY TEST
# wilcox test is appropriate for random idstribution - it compares two means
wilcox.test(sqrt2_seeded,sqrt2_unseeded)
# p=0.013 - we reject H0 that populations are equal - wilcox test didn't change

# KOLMOGOROV-SMIRNOV
ks.test(sqrt2_seeded,sqrt2_unseeded)
# p=0.019 - H0 rejected - ks test didn't change