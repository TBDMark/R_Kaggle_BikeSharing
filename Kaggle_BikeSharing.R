#Summary of data
{
datetime - hourly date + timestamp  
season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
holiday - whether the day is considered a holiday
workingday - whether the day is neither a weekend nor holiday
weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
temp - temperature in Celsius
atemp - "feels like" temperature in Celsius
humidity - relative humidity
windspeed - wind speed
casual - number of non-registered user rentals initiated
registered - number of registered user rentals initiated
count - number of total rentals
}

#tutorial here: http://beyondvalence.blogspot.com/2014/06/predicting-capital-bikeshare-demand-in.html
#load libraries
library(xts)
library(gbm)

#Load data
sampleSubmission <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/sampleSubmission.csv")
View(sampleSubmission)

test <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/test.csv")
View(test)

train <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/train.csv", stringsAsFactors=FALSE)
View(train)

summary(test)
summary(train)

str(test)
str(train)

# set categorical variables ####
# season, holiday, workingday, weather
train$season <- factor(train$season, c(1,2,3,4), ordered=FALSE)
train$holiday <- factor(train$holiday, c(0,1), ordered=FALSE)
train$workingday <- factor(train$workingday, c(0,1), ordered=FALSE)
train$weather <- factor(train$weather, c(4,3,2,1), ordered=TRUE)
# set datetime ####
train$datetime <- as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")
str(train)


# count is our target variable
# plot count distribution by categorical var
layout(matrix(c(1,1,2,3,4,5),2,3,byrow=FALSE))
boxplot(train$count, main="count")
boxplot(train$count ~ train$weather, main="weather")
boxplot(train$count ~ train$season, main="season")
boxplot(train$count ~ train$holiday, main="holiday")
boxplot(train$count ~ train$workingday, main="workingday")

# by numeric var
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot(train$temp, train$count, main="temp")
plot(train$atemp, train$count, main="feelslike temp")
plot(train$windspeed, train$count, main="windspeed")
plot(train$humidity, train$count, main="humidity")


# pairwise plot
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
 {
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   r <- abs(cor(x, y))
   txt <- format(c(r, 0.123456789), digits = digits)[1]
   txt <- paste0(prefix, txt)
   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
   text(0.5, 0.5, txt, cex = cex.cor * r)
 }
pairs(~count+season+holiday+workingday+weather+temp+humidity+windspeed,
       data=train, 
       main="Bike Sharing",
       lower.panel=panel.smooth, 
       upper.panel=panel.cor)


# plot
# count of bike rentals by datetime
plot(train$datetime, train$count, type="l", lwd=0.7,
     main="Count of Bike Rentals")
 
# percentage of registered users
percentage <- train$registered/train$count*100
plot(train$datetime, percentage, type="l", 
     main="Percentage of Registered Users")

summary(percentage)


#PART 2

names(train)

# running linear regression model
# exclude #casual and # registered
train.lm <- lm(count ~ ., data=train[,-c(10,11)])
summary(train.lm)
plot(train.lm)

# RMSLE-like values for various predictions
p <- seq(1,100, by=1)
a <- rep(40,100)
rmsle <- ((log(p+1)-log(a+1))^2)^0.5
plot(p,rmsle, type="l", main="Basic RMSLE Pattern", xlab="Predicted")
     
# total hours in training set
nrow(train)
 
# predict using 1000 samples
i.test <- sample(1:nrow(train), 1000)
# create test variables
test.1 <- train[i.test,1:9]
# create test target variable
test.1.target <- train[i.test,12]
# predict $count using test variables
test.1.pred <- predict(train.lm, newdata=test.1)
 
summary(test.1.pred) # has negatives

# n, number of test samples
n <- 1000
# eliminate negative predictions
test.1.pred[test.1.pred<0] <- 0
 
summary(test.1.pred)
 
# root mean squared log error (RMSLE)
# [(1/n)*sum(log(p+1)-log(a-1))^2]^0.5
# n = number of hours
# p = predicted count; a = actual count
# log = ln

(test.1.rmsle <- ((1/n)*sum(log(test.1.pred+1)-log(test.1.target+1))^2)^0.5)

#PART 3

# load gbm library
library(gbm)
# make sure to set working directory

# load training and test data
load("train.rdata")
test <- read.csv("test.csv")

# gbm -base model ####
genmod<-gbm(train$count~.
             ,data=train[,-c(1,9,10,11)] ## registered,casual,count columns
             ,var.monotone=NULL # which vars go up or down with target
             ,distribution="gaussian"
             ,n.trees=1200
             ,shrinkage=0.05
             ,interaction.depth=3
             ,bag.fraction = 0.5
             ,train.fraction = 1
             ,n.minobsinnode = 10
             ,cv.folds = 10
             ,keep.data=TRUE
             ,verbose=TRUE)

# best iteration
# cv
best.iter <- gbm.perf(genmod,method="cv") ##the best iteration number
print(pretty.gbm.tree(genmod, best.iter))

summary(genmod, n.trees=best.iter)

#Creating Results Code

# predict test data ####
# use best iteration from cv
pred.test <- predict(genmod, test[,-c(1,9)], best.iter, type="response")
summary(pred.test)

pred.test[pred.test<0] <- 0
# create output file
output <- data.frame(datetime=test[,1], count=pred.test)
write.csv(output, file="results.csv", quote=FALSE, row.names=FALSE)
