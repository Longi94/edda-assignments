################################
# point 1
################################
m = 30
n = 30
mu = 180
nu = seq(175,185,by=0.1)
sd = 5

B = 1000
step = 1
result = numeric(length(nu))
for (nu_i in nu) {
  p = numeric(B)
  for (b in 1:B) {
    x = rnorm(m,mu,sd)
    y = rnorm(n,nu_i,sd)
    p[b] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  result[step] = mean(p)
  
  step = step+1
}

plot(nu,result) # looks like normal distribution

################################
# point 2
################################
m = 100
n = 100
mu = 180
nu = seq(175,185,by=0.1)
sd = 5

B = 1000
step = 1
result = numeric(length(nu))
for (nu_i in nu) {
  p = numeric(B)
  for (b in 1:B) {
    x = rnorm(m,mu,sd)
    y = rnorm(n,nu_i,sd)
    p[b] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  result[step] = mean(p)
  
  step = step+1
}

plot(nu,result) # looks like normal distribution but more narrow (same effect when you decrease sd in normal distribution in comparison to previous plot)

# KOKAR my comment: we increased a number of man and woman => we increased number of samples in t-test 
#        => for each nu t-test has now bigger confidence because we have increased number of samples
#           more samples => t-test is more likely to reject null hypothesis near actual fact 
#           (we assume that populations are equal, but in fact they are equal only when height of women population (nu) is 180, 
#           for nu=178 and n=30 t-test has less confidence of rejecting this hypothesis than for n=100)


################################
# point 3
################################
m = 30
n = 30
mu = 180
nu = seq(175,185,by=0.1)
sd = 100

B = 1000
step = 1
result = numeric(length(nu))
for (nu_i in nu) {
  p = numeric(B)
  for (b in 1:B) {
    x = rnorm(m,mu,sd)
    y = rnorm(n,nu_i,sd)
    p[b] = t.test(x,y,var.equal=TRUE)[[3]]
  }
  result[step] = mean(p)
  
  step = step+1
}

plot(nu,result) # result looks like uniform distribution

# KOKAR my comment: now we have very large sd (standard deviation) and very low amount of samples (m=n=30)
# in such a situation t-test has very low confidence (each performed t-test is based on very wide normal distribution)

