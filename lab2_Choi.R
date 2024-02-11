#Project name: Project-2
#Author: Sujung Choi
#Date last edited: 09-30-2023

#1.
#(a)
attach(gulfprop)
#rename the columns
colnames(gulfprop)[1] ="list1"
colnames(gulfprop)[2] ="sale1"
colnames(gulfprop)[3] = "day1"
colnames(gulfprop)[4] ="list2"
colnames(gulfprop)[5] ="sale2"
colnames(gulfprop)[6] = "day2"

summary(gulfprop)
#gulf view

var(list1)
var(sale1)
var(day1)
sd(list1)
sd(sale1)
sd(day1)

boxplot(list1, horizontal = T, main = "Boxplot of List Price (Gulf View Condominiums)", xlab="List Price (in $K)")
boxplot(sale1, horizontal = T, main = "Boxplot of Sale Price (Gulf View Condominiums)", xlab="Sale Price (in $K)")
boxplot(day1, horizontal = T, main = "Boxplot of Days to sell (Gulf View Condominiums)", xlab="Days to sell")

par(mfrow=c(1,3)) #one row, three columns
#after this, the graphs will be in one picture

hist_boxplot(list1, col="lightblue", xlab="List Price (in $K)", freq=F, density = T)
hist_boxplot(sale1, col = "lightgreen", xlab="Sale Price (in $K)", freq=F, density = T)
hist_boxplot(day1, col = "lightyellow", xlab="Days to sell", freq=F, density = T)

#(b)
#no gulf view

#to ignore the NA
list2 = na.omit(list2)
sale2 = na.omit(sale2)
day2 = na.omit(day2)

boxplot(list2, horizontal = T, main = "Boxplot of List Price (No Gulf View Condominiums)", xlab="List Price (in $K)")
boxplot(sale2, horizontal = T, main = "Boxplot of Sale Price (No Gulf View Condominiums)", xlab="Sale Price (in $K)")
boxplot(day2, horizontal = T, main = "Boxplot of Days to sell (No Gulf View Condominiums)", xlab="Days to sell")

par(mfrow=c(1,3)) #one row, three columns
#after this, the graphs will be in one picture

hist_boxplot(list2, col="lightblue", xlab="List Price (in $K)", freq=F, density = T)
hist_boxplot(sale2, col = "lightgreen", xlab="Sale Price (in $K)", freq=F, density = T)
hist_boxplot(day2, col = "lightyellow", xlab="Days to sell", freq=F, density = T)

summary(gulfprop[,4:6])
var(list2)
var(sale2)
var(day2)

sd(list2)
s1= sd(sale2) # sample standard deviation for sale
s2 = sd(day2) # sample standard deviation for days

#(e)
salex = mean(sale2) # sample mean xbar for sale
dayx = mean(day2) # sample mean xbar for days
n=18 #sample size

#for sales
salex-qt(0.05/2,df=n-1, lower.tail = F)*s1/sqrt(n)# lower limit
salex+qt(0.05/2, df=n-1, lower.tail = F)*s1/sqrt(n)# upper limit

#for days
dayx-qt(0.05/2,df=n-1, lower.tail = F)*s2/sqrt(n)# lower limit
dayx+qt(0.05/2, df=n-1, lower.tail = F)*s2/sqrt(n)# upper limit

#(f)
#gulf view
s = sd(sale1)
me = 40

#me = 1.96 * s/sqrt(n)
n = (1.96*s/me)^2
print(ceiling(n)) #since sample size cannot have decimal, round up

#no gulf view
s1= sd(sale2)
me1 = 15
n1 = (1.96*s1/me1)^2
print(ceiling(n1))

#(g)
#gulf view list price $589000
#to estimate the final selling price
regression=lm(sale1~list1)
summary(regression)
# Estimated equation
#result1 = β0 + β1*list + u
result1 = -6.98456 + 0.97300 * 589
result1 #result1 = 566.1124

#to estimate the number of days
regression2=lm(day1~list1)
summary(regression2)
# Estimated equation
#day = β0 + β1*list + u
day = 89.84417 + 0.03408 * 589
day #day = 109.9173

#no gulf view list price $285,000
regression1=lm(sale2~list2)
summary(regression1)
# Estimated equation
#sale = β0 + β1*list + u
result = 13.98931 + 0.88907 * 285
result #result = 267.3743

#to estimate the number of days
regression3=lm(day2~list2)
summary(regression3)
# Estimated equation
#day = β0 + β1*list + u
day1 = 285.3067 -0.7063 * 285
day1 #day1 = 84.0112

detach(gulfprop)

#2.
#(a)
attach(professional)
summary(professional)
names(professional)

#rename the columns
colnames(professional)[3] ="purchase"
colnames(professional)[4] ="investment_value"
colnames(professional)[5] ="num_transaction"
colnames(professional)[6] ="broadband"
colnames(professional)[7] ="income"
colnames(professional)[8] ="children"

#frequency
freq1 = table(Gender)
freq1
freq2 = table(broadband)
freq2
freq3 = table(children)
freq3
freq4 = table(purchase)
freq4

#age
mean(Age)
var(Age)
sd(Age)

#investment value
mean(investment_value)
var(investment_value)
sd(investment_value)

#num_transaction
mean(num_transaction)
var(num_transaction)
sd(num_transaction)

#income
mean(income)
var(income)
sd(income)

#(b)
#CI for the mean age
x = mean(Age)
s = sd(Age)
n = 410
x-qt(0.05/2,df=n-1, lower.tail = F)*s/sqrt(n)# lower limit
x+qt(0.05/2, df=n-1, lower.tail = F)*s/sqrt(n)# upper limit

#CI for the mean household income
x1 = mean(income)
s1 = sd(income)
x1-qt(0.05/2,df=n-1, lower.tail = F)*s1/sqrt(n)# lower limit
x1+qt(0.05/2, df=n-1, lower.tail = F)*s1/sqrt(n)# upper limit

#CI for the proportion of subscribers who have broadband access
x = 256 #yes for broadband access
# confidence level=0.95
prop.test(x,n, conf.level = 0.95)
#95 percent confidence interval:
#  0.5753252 0.6710862

#CI for the proportion of subscribers who have children
x2 = 219 #yes to have children
prop.test(x2, n, conf.level =  0.95)
#95 percent confidence interval:
#0.4845521 0.5830908

#(d)
Age = professional$Age
investment_value=professional$investment_value
income = professional$income
broadband = professional$broadband
aggregate(investment_value ~ broadband, data= professional, FUN = mean)
aggregate(income ~ broadband, data= professional, FUN = mean)

detach(professional)

#3.
#(b)
attach(Project_2.data)
summary(Project_2.data)
x1=Project_2.data$x1
x2 = Project_2.data$x2
x3=Project_2.data$x3
x4=Project_2.data$x4

#x1
var(x1)
sd(x1)

#x2
var(x2)
sd(x2)

#frequency for categorical data
weight <- table(x3)
weight
height <-table(x4)
height
boxplot(x1, main="Boxplot of x1", xlab ="chest circumference (cm)", horizontal = T)
boxplot(x2, main="Boxplot of x2", xlab = "percent body fat using Brozek’s equation", horizontal = T)

#(c)
hist(Project_2.data$x1, main="Histogram of x1", xlab="chest circumference(cm)")
summary(x1)
hist(Project_2.data$x2, main="Histogram of x2", xlab="percent body fat")
summary(x2)
#(d)
plot(Project_2.data$x1,Project_2.data$x2, main="Scatter plot between x1 and x2", xlab="chest circumference(cm)", ylab="percent body fat")

#(e)
data = data[,-5]

Project_2.data$weight[Project_2.data$x3=="1"]="Heavy"
Project_2.data$weight[Project_2.data$x3=="0"]="Light"
Project_2.data$height[Project_2.data$x4=="1"]="Tall"
Project_2.data$height[Project_2.data$x4=="0"]="Short"
x3
freq5 = table(Project_2.data$weight)
freq5
barplot(freq5, main="Barplot for weight (lbs)")

freq6 = table(Project_2.data$height)
freq6
barplot(freq6, main="Barplot for height (inches)")

#(f)
n = 252
x = mean(x1)
s = sd(x1)
#CI for chest circumference (x1)
x-qt(0.05/2,df=n-1, lower.tail = F)*s/sqrt(n)# lower limit
x+qt(0.05/2, df=n-1, lower.tail = F)*s/sqrt(n)# upper limit

x = mean(x2)
s = sd(x2)
#CI for percent body fat (x2)
x-qt(0.05/2,df=n-1, lower.tail = F)*s/sqrt(n)# lower limit
x+qt(0.05/2, df=n-1, lower.tail = F)*s/sqrt(n)# upper limit

#t-test
t.test(x1)
t.test(x2)

#(g) compute 95% confidence interval for population variance
install.packages("DescTools")
library(DescTools)
x1ci= VarCI(x1, conf.level = 0.95) #for x1
x2ci= VarCI(x2, conf.level = 0.95) #for x2
x1ci
x2ci

#(h)
#t-test
t.test(x1, mu=0)
t.test(x2, mu=0)

#(i)
#t test on x2
t.test(x2 ~ x3)
t.test(x2 ~ x4)

#(j)
cor(x1, x2)

#(k)
cor.test(x1, x2)

#(l)
#test of independence (only for categorical variables)
#null: two categorical variables are independent
#alternative: two categorical variables are not independent 
chisq.test(x3, x4)

#(m)
model=lm(x2~x1)
summary(model)
# Estimated equation
#x2 = β0 + β1 * x1 + u
#x2=-46.21636 + 0.64622 * x1

detach(Project_2.data)