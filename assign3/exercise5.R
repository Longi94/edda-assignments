library(multcomp)
library(lme4)

data = read.table("nauseatable.txt", header=TRUE)
attach(data)

chlorpromazine_no_nausea = data[1,1]
chlorpromazine_nausea = data[1,2]
pentobarbital_100mg_no_nausea = data[2,1]
pentobarbital_100mg_nausea = data[2,2]
pentobarbital_150mg_no_nausea = data[3,1]
pentobarbital_150mg_nausea = data[3,2]

#1
nausea = c()
medicine = c()

for(i in 1:304){
  if(i <= 100){
    nausea[i] = 0
    medicine[i] = "chlorpromazine"
  }else if(i > 100 && i <= 152){
    nausea[i] = 1
    medicine[i] = "chlorpromazine"
  }else if(i > 152 && i <= 184){
    nausea[i] = 0
    medicine[i] = "pentobarbital 100mg"
  }else if(i > 184 && i <= 219){
    nausea[i] = 1
    medicine[i] = "pentobarbital 100mg"
  }else if(i > 219 && i <= 267){
    nausea[i]=0
    medicine[i] = "pentobarbital 150mg"
  }else if(i > 267 && i <= 304){
    nausea[i] = 1
    medicine[i] = "pentobarbital 150mg"
  }
}

nausea.frame = data.frame(nausea,medicine)

#2
xtabs(~medicine+naus)
#Looking at the output it looks very similar to the original table... previous data frame is therefore made correct. Not sure what the expected analysis is here.

#3
attach(nausea.frame)

B=1000
tstar=numeric(B)
for(i in 1:B){
  treatstar=sample(medicine)
  tstar[i] = chisq.test(xtabs(~treatstar+nausea))[[1]]
}

myt=chisq.test(xtabs(~medicine+nausea))[[1]]

pl=sum(tstar<myt)/B
pr=sum(tstar>myt)/B
pl
#pl is > 0.05, therefore we accept the null hypothesis that the different medicines work equally against nausea.