#Project name: Project-3
#Author: Sujung Choi
#Date last edited: 10-21-2023

#1
#(a), (b), (c),(d)
attach(Nyc)
summary(Nyc)

regression = lm(Price~Food+Decor+East+Service)
summary(regression)
#(e)
#add a column for residuals
#residuals show the difference between actual values and predicted values
Nyc$residu<-resid(regression)

detach(Nyc)

#2.
#(a)
attach(Nuclear_power)

plot(MWatts, Cost, main="Scatterplot of Cost vs Mwatts", xlab="Mwatts", ylab="Cost")
abline(lm(Cost~MWatts))

plot(Date, Cost, main="Scatterplot of Cost vs Date", xlab="Date", ylab="Cost")
abline(lm(Cost~Date))

cor(MWatts, Cost) #0.4717609
cor(Date, Cost) #0.6104543


#(b),(c)
#regression of Cost on MWatts
m5=lm(Cost~MWatts)
summary(m5)
plot(m5)
# Estimated equation
#y=111.7408+0.4238*x
est_cost = 111.7408+0.4238*1000
est_cost #535.5408

#regression of Cost on Date
m6 = lm(Cost~Date)
summary(m6)
plot(m6)

#predicted vector (Cost vs MWatts)
pred_m5 = predict(m5)
pred_m5

#adding predicted variable in the dataset
Nuclear_power$pred=Nuclear_power$pred=predict(m5)

# residual
residual = resid(m5)

Nuclear_power$residual=Nuclear_power$residual=resid(m5)

#residual vs fitted values (Cost vs MWatts)
plot(pred_m5, residual, ylab="Residual", xlab="Predicted value", main="Plot of Residual vs Fitted value (Cost vs MWatts)")
#all the values are close to 0 meaning they are consistent


#predicted vector (Cost vs Date)
pred_m6 = predict(m6)
pred_m6

#adding predicted variable in the dataset
Nuclear_power$pred2=Nuclear_power$pred2=predict(m6)

# residual
residual2 = resid(m6)

Nuclear_power$residual2=Nuclear_power$residual2=resid(m6)

#residual vs fitted values (Cost vs Date)
plot(pred_m6, residual2, ylab="Residual", xlab="Predicted value", main="Plot of Residual vs Fitted value")
#all the values are close to 0 meaning they are consistent

# we can use linear model
plot(MWatts, residual)

#to check normal distribution,
#normal probability plot (QQplot)
qqnorm(residual)
#all the values are close to the line, meaning the residual follows normal distribution
qqline(residual, col="red")

plot(Date, residual2)
qqnorm(residual2)
qqline(residual2, col="red")

#(d)
summary(m5)
summary(m6)

#(e)
#from m5
# Estimated equation
#y=111.7408+0.4238*x
est_cost = 111.7408+0.4238*1000
est_cost #535.5408

#(f)
Nuclear_power$residu<-resid(m5)
plot(Date, residu, main="Scatterplot of Residuals against Date", xlab = "Date", ylab="Residuals")

#(h)
#correlation coefficient
cor(MWatts, Date)

#scatter plot
plot(Date, MWatts, main="Scatterplot of Mwatts vs Date", xlab="Date", ylab="Mwatts (Power plant net capacity in megawatts)")

# Scatterplot using ggplot2
library(ggplot2)
ggplot(Nuclear_power, aes(x=Date, y=MWatts)) + geom_point() + 
  ggtitle("Scatterplot of Mwatts vs. Date") + 
  xlab("Date (years after 1900)") + 
  ylab("Mwatts (Power plant net capacity)")

detach(Nuclear_power)
#3.
#(a)
attach(alumnigiving)
summary(alumnigiving)
#rename the columns 
colnames(alumnigiving)[3] ="grad_rate"
colnames(alumnigiving)[4] ="percent_class"
colnames(alumnigiving)[5] ="stu_facul_ratio"
colnames(alumnigiving)[6] ="alumni_giving_rate"

sd(grad_rate)
sd(percent_class)
sd(stu_facul_ratio)
sd(alumni_giving_rate)

par(mfrow=c(2,2)) #one row, four columns
boxplot(grad_rate, col="lightblue", xlab="Graduation Rate", horizontal = T)
boxplot(percent_class, col = "lightgreen", xlab="% of Classes Under 20", horizontal=T)
boxplot(stu_facul_ratio, col = "lightyellow", xlab="Student/Faculty Ratio", horizontal= T)
boxplot(alumni_giving_rate, col="orange", xlab="Alumni Giving Rate", horizontal=T)

#(b)
#simple linear regression model to predict alumni giving rate based on graduation rate
model=lm(alumni_giving_rate ~ grad_rate)
summary(model)
#y = -68.7612 + 1.1805 * x

#(c)
m2=lm(alumni_giving_rate ~ grad_rate + percent_class + stu_facul_ratio)
summary(m2)
#y= -20.72013 + 0.74818 * x1 + 0.02904 * x2 + -1.19201 * x3

#(d)
m3 = lm(alumni_giving_rate ~ grad_rate + stu_facul_ratio)
summary(m3)

#(e)-ii, iii
plot(m2)

#extract residuals and fitted values
residuals <- m2$residuals
alumnigiving$residuals <- residuals
alumnigiving$fitted <- fitted(m2)

plot(fitted, residuals, main="Residuals vs. Fitted", xlab="Fitted values", ylab="Residuals")
abline(h = 0, lty = 2)  # adds a dashed line at y=0

#default is ascending order, so with - in front, it sorts in descending order, so the highest top 5
higher_universities = head(alumnigiving[order(-alumnigiving$residuals), ],n=5)
higher_universities

#this is ascending order so the lowest top 5
lower_universities = head(alumnigiving[order(alumnigiving$residuals), ], n=5)
lower_universities

# Label these points on the plot
text(x=higher_universities$fitted, y=higher_universities$residuals, labels=higher_universities$University, cex=0.5, pos=4)
text(x=lower_universities$fitted, y=lower_universities$residuals, labels=lower_universities$University, cex=0.5, pos=4)

#get the universities names for the result
head(higher_universities$University, n=5)
head(lower_universities$University, n=5)

detach(alumnigiving)

#4.
attach(lab_3)
#(a)
summary(lab_3)
cor(lab_3)

#(b)
m8 = lm(Price~Rooms)
summary(m8)

#confidence interval
confint(m8)

#(c)
m9 = lm(Price~`Home Size`+`Lot Size` + Rooms + Bathrooms)
summary(m9)
confint(m9)

detach(lab_3)

#5.
#(a)
attach(vintage_new)
#time series
#first parameter is dependent variable
#col is for color
plot.ts(Sales, main="Time Series Plot", col=4)

# install the packages
install.packages("ggplot2")
library(ggplot2)

#x-axis+geom_line y-axis
ggplot(vintage_new, aes(x=Month))+geom_line(aes(y=Sales))+labs(y="Sales", x="Month")+ggtitle("Time series")

#(b)
#seasonality without trend
#using dummy variables
model5=lm(Sales~D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11)
summary(model5)
#y=223.667 + 38.667 * D1 + 19 * D2 + 24.333 * D3 + -31.667 * D4 -28 * D5 -74 * D6 -67.667 * D7 -61.333 * D8 -104.333 * D9 -87.667 * D10 -59.667 * D11

#seasonality with trend
model6=lm(Sales~D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+Month)
summary(model6)
#y=199.25 + 49.85764 * D1 + 29.17361 * D2 + 33.48958 * D3 -23.52778 * D4 -20.87847 * D5 -67.89583 * D6 -62.57986 * D7 -57.26389 * D8 -101.28125 * D9 -85.63194 * D10 -58.64931*D11 + Month*1.01736

 
#January
y=223.667 + 38.667 * 1 
y #262.334

#Feb
y=223.667 + 19 * 1 
y

#Mar
y=223.667 + 24.333 
y

#April
y=223.667 -31.667 
y

#May
y=223.667 -28 
y

#June
y=223.667 -74 
y

#July
y=223.667 -67.667 
y

#Aug
y=223.667 -61.333 
y

#Sep
y=223.667 -104.333 
y

#Oct
y=223.667 -87.667 
y

#Nov
y=223.667 -59.667
y

#Dec
y=223.667

#actual value - forecast value = forecast error
295000 - 262334 = 32666

#get using model6
#fourth year's January means 37th month
y=199.25 + 49.85764 + 37*1.01736 #y=286.75
295000 - 286750 = 8250

detach(vintage_new)