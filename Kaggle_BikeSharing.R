#Load data
sampleSubmission <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/sampleSubmission.csv")
View(sampleSubmission)

test <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/test.csv")
View(test)

train <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/train.csv")
View(train)

summary(test)
summary(train)

str(test)
str(train)

# Add dummy values to test dataframe
test$casual = 0
test$registered = 0
test$count = 0

# Bind train and test data together
cdata = rbind(train, test)

# Convert some features to factors
cdata$season = as.factor(cdata$season)
cdata$holiday = as.factor(cdata$holiday)
cdata$workingday = as.factor(cdata$workingday)
cdata$weather = as.factor(cdata$weather)

#split Date/Time fields
datetime = as.POSIXlt(cdata$datetime)
hour = datetime$hour
weekday = as.factor(datetime$wday)
month = as.factor(datetime$mon)
year = 1900 + datetime$year
cdata$datetime = datetime

# Add the new features to the combined dataframe
cdata = cbind(cdata, hour, weekday, month, year)
# Split in the corresponding train/test datasets
train = cdata[0:10886,]
test = cdata[10887:17379,]

#proportions of hours
table(cdata$hour)
summary(cdata$hour)
prop.table(table(cdata$hour))

#Random Forests analysis
library(randomForest)

fit <- randomForest(count ~ hour + weekday + month + year + season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data=cdata, importance=TRUE, ntree=25)
varImpPlot(fit)

#submission
prediction <- predict(fit,test)
submit <- data.frame(datetime = test$datetime, count = count)
write.csv(submit, file = "Kaggle_BikeSharingv1", rownames = FALSE)