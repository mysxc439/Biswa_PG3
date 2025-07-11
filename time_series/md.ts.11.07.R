rm(list = ls())
data1= co2;data1
#years = floor(time(data))
#yearly_avg = as.data.frame(tapply(data,years,mean),colnames=c("year","avg"));yearly_avg
#model = lm(yearly_avg$`tapply(data, years, mean)`~ unique(years))
#summary(model)

avg1 = aggregate(data1,nfrequency = 1,mean);avg1
t1 = 1:length(avg1);t1
model1 = lm(avg1~t1);summary(model1)
plot(t1, avg1, type ="l", xlab="Time", ylab="Average CO2", main="Yearly Average CO2 Levels")
abline(model1, col = "red")  # regression line  
#plot(model1,1)  # residual vs fitted

data2= AirPassengers; data2
avg2= aggregate(data2,nfrequency=1,mean);avg2
t2 = 1:length(avg2);t2
model2 = lm(avg2~t2);summary(model2)
plot(t2, avg2, type ="l", xlab="Time", ylab="Average passengers", main="Yearly Average passengers")
abline(model2, col = "red")  # regression line
#plot(model2, 1)  # residual vs fitted

data3= JohnsonJohnson; data3
avg3= aggregate(data3,nfrequency=1,mean);avg3
t3 = 1:length(avg3);t3
model3 = lm(avg3~t3);summary(model3)
plot(t3, avg3, type ="l", xlab="Time", ylab="Average Earnings(dollars)", main="Yearly Average earnings(dollar)")
abline(model3, col = "red")  # regression line 
model3_quad = lm(avg3~t3+I(t3^2));summary(model3_quad)
lines(t3, model3_quad$fitted.values, col = "blue")  # add quadratic regression line to the plot
#plot(model3, 1)  # residual vs fitted
#plot(model3_quad,1)
