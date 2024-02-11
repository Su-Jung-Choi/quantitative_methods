#Project name: Project-1
#Author: Sujung Choi
#Date last edited: 09-11-2023

# 1.
#change the name of second column
colnames(doctordata)[2] ="doctors"

#(b) create a graph
boxplot(doctordata$doctors, horizontal = T, main="Boxplot of Health Care Availability", xlab="Number of doctors (per 100,000)")
summary(doctordata)
IQR<-IQR(doctordata$doctors)
Q3<-249.5
outlier_line = q3+1.5*IQR
Q1 <- 202.5
#(c) mean and median
describe(doctordata)
summary(doctordata)

#(d)
#find outliers
boxplot(doctordata$doctors, plot=FALSE)$out
outliers <- boxplot(doctordata$doctors, plot=FALSE)$out
outliers

#remove outliers
x<-doctordata
x<- x[-which(x$doctors %in% outliers),]
summary(x)
boxplot(x$doctors, names.arg = x$State, main="Barplot of Health Care Availability", ylab="Number of doctors (per 100,000)", xlab="State", las=2 )

#create new data without outliers
#1) removing manually
#newdata<-doctordata[-c(39,7,20,32,21,51),]
#newdata
#summary(newdata)

#2) use the following formula to create a new data using subset function to remove outliers
#data = subset(olddata, var > Q1 - 1.5*	iqr & var < Q3+1.5*iqr)
newdata = subset(doctordata, doctors > Q1 - 1.5*IQR & doctors < Q3+1.5*IQR)

#(e) construct boxplot of doctors
boxplot(doctordata$doctors, main="Boxplot of doctors")
boxplot(newdata$doctors, horizontal = T, main="Boxplot of doctors", xlab="Number of doctors (per 100,000)")

# 2.
boxplot(Weight~Position, data=football)

# 3.
colnames(patientwaits)[1] ="without"
colnames(patientwaits)[2] ="with"

#(a)
mean(patientwaits$with)
median(patientwaits$with)
mean(patientwaits$without)
median(patientwaits$without)

#(b)
var(patientwaits$with)
sd(patientwaits$with)
var(patientwaits$without)
sd(patientwaits$without)

# (c)
boxplot(patientwaits$without, horizontal = T, main="Boxplot of Waiting times without a wait-tracking system", xlab="minutes")

# (d)
boxplot(patientwaits$with, horizontal = T, main="Boxplot of Waiting times with a wait-tracking system", xlab="minutes")

# (e)
summary(patientwaits$with)
summary(patientwaits$without)

# 4.
# (a)
boxplot(Job_growth$Growth_in_percent, horizontal = T, main="Boxplot of Predicted Growth Rates")

# (b)
mean(Job_growth$Growth_in_percent)
median(Job_growth$Growth_in_percent)

# (d)
sd(Job_growth$Growth_in_percent)
IQR(Job_growth$Growth_in_percent)
summary(Job_growth)

#(f)
#remove Las Vegas
x<-Job_growth
x<- x[-1,]
summary(x)
sd(x$Growth_in_percent)
IQR(x$Growth_in_percent)
boxplot(x$Growth_in_percent, horizontal = T, main="Boxplot of Predicted Growth Rates")

# 5.
color <-c("Silver", "White", "Black", "Blue", "Green", "Medium red", "Brown", "Gold", "Bright red", "Grey")
percent <-c(21.0, 15.6, 11.2, 9.9, 13.2, 6.9, 5.6, 4.5, 4.3, 2.0)

#(a)
100-sum(percent)

#(b)
barplot(percent, names.arg = color, main="Barplot of car's color", xlab="color", ylab="percent(%)")
#for pie chart
color <-c("Silver", "White", "Black", "Blue", "Green", "Medium red", "Brown", "Gold", "Bright red", "Grey", "other")
percent <-c(21.0, 15.6, 11.2, 9.9, 13.2, 6.9, 5.6, 4.5, 4.3, 2.0, 5.8)
pie(percent, labels=color, main="Pie Chart of car's color")

# 6.
#(a)
intended_major<-table(major$Major)
intended_major

#(b)
year<-table(major$`college(in year)`)
year
barplot(year, ylab="frequency", xlab="Year in college", main="Bar Graph", col=c("Red", "Blue", "Green", "Yellow"))

#(c)
colnames(major)[3] ="height"
boxplot(data=major, height~Gender, ylab="Height (inches)", main="Boxplot of Height in Inches by gender")
#summary of height by sex (female and male)
aggregate(major$height,by=list(major$Gender),summary)