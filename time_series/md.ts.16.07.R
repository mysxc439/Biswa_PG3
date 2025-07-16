rm(list=ls())
data=co2 
trend = aggregate(data,nfrequency = 1, mean);trend
t = 1:length(data);t
model = lm(data~t);summary(model)
a0= as.numeric(model$coefficients[1]);a0
a1= as.numeric(model$coefficients[2]);a1

time = 1  # for january,1959
# Tt = a0+(a1/12)*(time+0.5);Tt  # origin: december,1958, unit: 1month
Tt = a0+(a1/12)*(time+5+0.5);Tt  # origin: july,1958, unit: 1month
