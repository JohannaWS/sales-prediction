
### SETUP ###
#https://www.kaggle.com/datasets/rutuspatel/walmart-dataset-retail
setwd("C:\\Users\\johan\\Documents\\GitHub\\sales-prediction\\data")
sales=read.csv("Walmart_Store_sales.csv")
attach(sales)


### IMPORTS ###
library (gbm)
library(caret)
library(weathermetrics)
library(lubridate)
library(corrplot)
library(party)

### FUNCTIONS ###
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
rsq <- function (x, y) cor(x, y) ^ 2

### DATA INSIGHTS ###

colnames(sales)
str(sales)
sales= sales[order(Store, Date),]

## explanatory variables ##

#missing values? no!
unique(is.na(sales))

# DATE
Date= as.Date(Date, "%d-%m-%Y")
Date_for= as.Date(Date, "%d-%m-%Y")[c(1:143)]
summary(Date)
#when are sales recorded? always on Friday!
table(wday(Date, week_start=1, label=TRUE))
unique(Date)

#any records missing? no! each store has a record each week
sort(Date)
timediffs=list()
for(i in seq(length(Date)-1)){
  diff=difftime(Date[i], Date[i+1])[[1]]
  timediffs[length(timediffs)+1]=diff
}

# STORE
summary(Store)

# HOLIDAY
hist(Holiday_Flag)
summary(Holiday_Flag)

# TEMPERATURE
Temperature.Cel=fahrenheit.to.celsius(Temperature, round = 2)
hist(Temperature.Cel, main="Distribution of Temperature", freq = FALSE, col="darkgreen", xlab="Temperature in °C")
summary(Temperature.Cel)

temp.per.store=list()
plot(Temperature.Cel[Store==1],type="l", col="blue", lty=1, ylim=c(-20,40), main = "Average Weekly Temperature by Store", ylab = "Weekly Temperature", xlab="Time", x = Date_for)
for( st in seq(2,45)){
  temp.per.store[st]=summary(Temperature.Cel[Store==st])[4]
  lines(Temperature.Cel[Store==st], col=st, x = Date_for)
}

# FUEL
summary(Fuel_Price)
hist(Fuel_Price, col="darkgreen", main="Distribution of Fuel Price", xlab = "Fuel Price")

plot(Fuel_Price[Store==1],type="l", col="blue", lty=1, ylim=c(2,5), main = "Fuel Price by Store", ylab = "Price", xlab="Time", x = Date_for)
for( st in seq(2,45)){
  lines(Fuel_Price[Store==st], col=st, x = Date_for)
}

# UNEMPLOYMENT
hist(Unemployment)
df_unemp=subset(sales, select = c("Unemployment", "Date", "Store"))
plot(Unemployment[Store==1],type="l", col="blue", lty=1, ylim=c(4,15), main = "Unemployment by Store", ylab = "Weekly Unemployment Rate", xlab="Time",  x = Date_for)

for( st in seq(2,45)){
  lines(Unemployment[Store==st], col=st,  x = Date_for)
}

summary(Unemployment)

# CPI
summary(CPI)
hist(CPI, breaks = 20, col="darkgreen")

plot(CPI[Store==1],type="l", col="blue", lty=1, main = "CPI by Store", ylab = "Weekly CPI", xlab="Time", ylim=c(120,225))
for( st in seq(2,45)){
  lines(CPI[Store==st], col=st)
}


## response variable ##
boxplot(Weekly_Sales , col="darkgreen", main="Supermarket", ylab="Weekly Sales")
hist(Weekly_Sales, col="darkgreen", main="Supermarket", xlab="Weekly Sales")
summary(Weekly_Sales)

plot(Weekly_Sales[Store==1],type="l", col="blue",ylim = c(20000,3500000), lty=1, main = "Weekly Sales by Store", ylab = "Weekly Sales", xlab="Time", x = Date_for)
for( st in seq(2,45)){
  lines(Weekly_Sales[Store==st], col=st, x = Date_for)
}


## New column ##

# Change in Weekly Sales
Diff_in_Sales=list()
sales= sales[order(Store, Date),]
for( st in seq(1,45)){
  diff= c(0,diff(Weekly_Sales[Store==st])/Weekly_Sales[Store==st][c(2:143)])
  Diff_in_Sales=append(Diff_in_Sales,as.numeric(diff))
}

Diff_in_Sales=unlist(Diff_in_Sales)
hist(Diff_in_Sales, col="darkgreen")


plot(Diff_in_Sales[Store==1],type="l", col="blue", lty=1, main = "Change in Sales by Store", ylab = "Weekly Change in Sales", xlab="Time", x=Date_for,ylim=c(-2,1) )
for( st in seq(2,45)){
  lines(Diff_in_Sales[Store==st], col=st, x=Date_for)
}

summary(Diff_in_Sales)
sales[Diff_in_Sales<=-1,]
sales[Diff_in_Sales>=0.3,]

### CORRELATIONS ###

cor=cor(sales[c(3:8)])
testRes = cor.mtest(cor, conf.level = 0.95)
colnames(cor)=c("Weekly Sales","Holiday","Temperature","Fuel Price","CPI","Unemployment")
rownames(cor)=c("Weekly Sales","Holiday","Temperature","Fuel Price","CPI","Unemployment")

corrplot(cor, type = 'upper', method = 'ellipse',addCoef.col = 'black', diag = FALSE,sig.level = 0.05, insig = "pch")
pairs(sales[c(3:8)])
summary(sales)


#### MODELLING ###
Date=as.numeric(Date)
sales$Date=as.numeric(Date)

#Set train and test
set.seed(42)
train = sample (1:nrow(sales), 0.7*nrow(sales))
sales.train=sales[train ,]
sales.test=sales[-train ,]

### BASE MODEL ###
m1 <- lm(sales.train$Weekly_Sales~., data=sales.train)
summary(m1)
pred1=predict.lm(m1,sales.test)
plot(pred1,sales.test$Weekly_Sales, xlab = "Predicted Values for Sales", ylab="Actual Values for Sales", main="Predicted vs. Actual Weekly Sales")
abline(0,1, col="red")
dev.lm <- sum((pred1-sales.test$Weekly_Sales)^2)
dev.lm
rsq(pred1, sales.test$Weekly_Sales)
AIC(m1)


### IMPROVED LINEAR MODEL ###
#transform data: log, exp, srt, 
# no improvement

#hist((norm.sales$Unemployment))
#hist((norm.sales$Temperature))
#hist((sales$Fuel_Price))
#hist((sales$CPI))

#normalized data by store
norm.sales=sales
for( st in seq(1,45)){
  norm <- as.data.frame(lapply(norm.sales[Store==st,], normalize))
  norm.sales[Store==st,]=norm
}

norm.sales=norm.sales[-1]
pairs(norm.sales)

cor=cor(norm.sales[c(2:7)])
testRes = cor.mtest(cor, conf.level = 0.95)
colnames(cor)=c("Weekly Sales","Holiday","Temperature","Fuel Price","CPI","Unemployment")
rownames(cor)=c("Weekly Sales","Holiday","Temperature","Fuel Price","CPI","Unemployment")
corrplot(cor, type = 'upper', method = 'ellipse',addCoef.col = 'black', diag = FALSE,sig.level = 0.05, insig = "pch")
#corrplot(cor, type = 'upper', method = 'ellipse',p.mat = testRes$p,addCoef.col = 'black', diag = FALSE,sig.level = 0.05, insig = "pch")

# test and train for normalized data
set.seed(42)
train = sample(1:nrow(norm.sales), 0.7*nrow(norm.sales))
norm.sales.train=norm.sales[train ,]
norm.sales.test=norm.sales[-train ,]

m2 <- lm(norm.sales.train$Weekly_Sales~., data=norm.sales.train)
summary(m2)

pred.norm=predict.lm(m2,norm.sales.test)
plot(pred.norm,norm.sales.test$Weekly_Sales, xlab = "Predicted Values for Sales", ylab="Actual Values for Sales", main="Predicted vs. Actual Weekly Sales")
abline(0,1,col="red")
AIC(m2)
rsq(pred.norm,norm.sales.test$Weekly_Sales)

# Stepwise Regression: no considerable improvement
m3 <- step(m2, direction="both")
summary(m3)
plot(m3)


##### DECISION TREE ####

sales$Store=factor(sales$Store)

model<- ctree(Weekly_Sales ~., sales.train)
plot(model)
pred<-predict(model, sales.test)
plot(pred,sales.test$Weekly_Sales ,xlab = "Predicted Values for Sales", ylab="Actual Values for Sales", main="Predicted vs. Actual Weekly Sales")
abline(0,1, col="red")

dev.dt1 <- sum((pred-sales.test$Weekly_Sales)^2)
dev.dt1

rsq(pred,sales.test$Weekly_Sales)

summary(model)


#### DECISION TREE: Normalized Data ####

model<- ctree(norm.sales.train$Weekly_Sales ~., norm.sales.train)
plot(model)
pred<-predict(model, norm.sales.test)
plot(pred, norm.sales.test$Weekly_Sales)
abline(0,1, col="red")

dev.dt2 <- sum((pred-norm.sales.test$Weekly_Sales)^2)
dev.dt2
rsq(pred,norm.sales.test$Weekly_Sales)

#AIC(model)


#### Gradient Boosting ####
numberTrees=2000
depth=3
learning=0.01

# default vector of paramters
mai.old=par()$mai
mai.new<-mai.old
mai.new[2] <- 2.1 
par(mai=mai.new)


boost.sales=gbm(Weekly_Sales ~ ., data=sales.train, n.trees=numberTrees, interaction.depth=depth, shrinkage = learning)
#training error
plot(boost.sales$train.error, type="l", ylab="Training Error", main="Training Error with Learning Rate of 0.01")
#relative influence plot
summary(boost.sales, las=1, main="Influence Plot") 
#prediction
yhat.boost=predict(boost.sales, newdata=sales.test, n.trees=1:numberTrees)
err = apply(yhat.boost, 2, function(pred) mean((sales.test$Weekly_Sales - pred)^2))
plot(err, type="l")
r2 = apply(yhat.boost, 2, function(pred) cor(pred,sales.test$Weekly_Sales)^2)
plot(r2, type="l", xlab = "Number of Trees", ylab="R^2", main="Performance of GBM")
plot(yhat.boost[,numberTrees],sales.test$Weekly_Sales, xlab = "Predicted Values for Sales", ylab="Actual Values for Sales", main="Predicted vs. Actual Weekly Sales")
abline(0,1, col="red")
# error comparison
plot(boost.sales$train.error, type="l", xlab="Number of Trees", ylab="Error (MSE)", main="Error in Test vs. Train")
legend(1000,250000000000,legend=c("Train", "Test"),col=c(1, 2), lty=1:1)
lines(err, type="l", col=2)
#minimum error in test set
best=which.max(r2)
abline(v=best, lty=2, col=4)
# performance
min(err) #minimum error
max(r2) #maximum r2


# partial dependence plots
plot(boost.sales, i.var=1, n.trees = best) #store: factor
plot(boost.sales, i.var=2, n.trees = best) #date
plot(boost.sales, i.var=3, n.trees = best) #holiday: binary
plot(boost.sales, i.var=4, n.trees = best) #bivariate
plot(boost.sales, i.var=5, n.trees = best) #fuel price
plot(boost.sales, i.var=6, n.trees = best, main="Partial Dependence of CPI", ylab="Weekly Sales") #cpi
plot(boost.sales, i=7, n.trees = best, main="Partial Dependence of Unemployment Rate", ylab="Weekly Sales")# unemployment


