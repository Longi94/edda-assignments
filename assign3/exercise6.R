data = read.table("airpollution.txt", header=TRUE)

#######################
# POINT 1
#######################
pairs(data, panel=panel.smooth)

#some pairs are highly correlated, especially with oxidant that is a main factor of our investigation
#I think linear regression could be useful here

#######################
# POINT 2
#######################

#### wind ####
windlm=lm(oxidant~wind,data=data)
summary(windlm)
plot(oxidant~wind, data=data)
abline(windlm)
#comment: slope is negative, p-value is low - there is significant correlation

#### temperature ####
templm=lm(oxidant~temperature,data=data)
summary(templm)
plot(oxidant~temperature, data=data)
abline(templm)
#comment: slope is positive, p-value is low - there is significant correlation

#### humidity ####
humilm=lm(oxidant~humidity,data=data)
summary(humilm)
plot(oxidant~humidity, data=data)
abline(humilm)
#comment: slope is negative, p-value is high - there is no significant correlation

#### insolation ####
insolm=lm(oxidant~insolation,data=data)
summary(insolm)
plot(oxidant~insolation, data=data)
abline(insolm)
#comment: slope is positive, p-value is low - there is significant correlation


# DETERMINATION COEFFICIENT: R^2
# wind - 0.5863
# temperature - 0.576
# humidity - 0.124 - high p-value H0 rejected
# insolation - 0.22993


# STEP-UP

#the best - the highest R^2: wind

# wind + temperature
windTemplm=lm(oxidant~wind+temperature,data=data)
summary(windTemplm)
# R2: 0.7773 - signifficant increase

# wind + temperature + insolation
windTempInslm=lm(oxidant~wind+temperature+insolation,data=data)
summary(windTempInslm)
# R2: 0.7816 - insignifficant increase, we should stop

#######################
# POINT 3
#######################

#included all relevant variables
windTempInslm=lm(oxidant~wind+temperature+insolation,data=data)
summary(windTempInslm)

#for insolation p-value is higher than 0.05 - we remove that variable
windTemplm=lm(oxidant~wind+temperature,data=data)
summary(windTemplm)

