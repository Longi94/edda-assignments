light1879 = scan("light1879.txt")
light1882 = scan("light1882.txt")
lightnc = scan("light.txt")

# convert to the proper km/s value
light1879 = light1879 + 299000
light1882 = light1882 + 299000
lightnc = 7442000 / (lightnc / 1000 + 24.8)

# 1.
hist(light1879)
hist(light1882)
hist(lightnc)

boxplot(light1879)
boxplot(light1882)
boxplot(lightnc)

# first two look like normal distribution, the third on looks more like lognormal, I don't know what else to say?

# 2.

# confidence interval of light1879
# result: 299836.9 299867.9
error1 = qnorm(0.975) * sd(light1879) / sqrt(length(light1879))
T1 = mean(light1879)
c(T1 - error1, T1 + error1)

# confidence interval of light1882
# result: 299712.4 299800.0
error2 = qnorm(0.975) * sd(light1882) / sqrt(length(light1882))
T2 = mean(light1882)
c(T2 - error2, T2 + error2)

# confidence interval of lightnc
# result: 299732.5 299789.3 
B = 1000
Tstar = numeric(B)
for (i in 1:B) {
  Xstar = sample(lightnc, replace = TRUE)
  Tstar[i] = mean(Xstar)
}
Tstar25 = quantile(Tstar, 0.025)
Tstar975 = quantile(Tstar, 0.975)

T3 = mean(lightnc)
c(2 * T3 - Tstar975, 2 * T3 - Tstar25)

# 3.
# no idea what to say about them, they are pretty close to the speed of light *shrug*

# 4.
speed_of_light = 299792.458
# outside of confidence interval of light1879 and lightnc but inside of confidence interval of light1882
