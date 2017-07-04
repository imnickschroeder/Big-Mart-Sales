# Big Mart sales prediction challenge

# I will be using the tutorial from analytics vidhya as a reference
# Here is the URL for the tutorial: https://www.analyticsvidhya.com/blog/2016/02/bigmart-sales-solution-top-20/
# Note: The tutorial is in Python, I will be using R

# Read in the data:
train <- read.csv("~/R_Projects/bigmart_sales_prediction/bigmart_train.csv",na.strings=c("","NA"))
test <- read.csv("~/R_Projects/bigmart_sales_prediction/bigmart_test.csv",na.strings=c("","NA"))

# Let's combine the two datasets for data exploration and feature engineering
# Create the item outlet sales variable for test so we can merge
test$Item_Outlet_Sales <- NA
# Create a new column called source, so that we will remember which data set it was originally in
train$Source <- 'train'
test$Source <- 'test'
data <- rbind(train,test)

# Let's start with finding which columns have missing values
sapply(data, function(x) sum(is.na(x)))
 # We can see that Item_Weight, and Outlet_Size have missing values that will be dealt with later

# Let's look at some basic summary statistics
summary(data)
# Let's look more in depth at the factor variables
library(plyr)
count(data,'Item_Identifier')
count(data,'Item_Fat_Content') # We can see that some items are incorrectly coded between LF, low fat, and Low Fat & reg and regular
count(data,'Outlet_Location_Type')
count(data,'Outlet_Size')
count(data,'Outlet_Type')

# Data cleaning: imputing missing values, treating outliers
# There are two variables that have missing values: Item_Weight and Outlet_Size
# Let's take care of Item_Weight first by taking the average weight of a particular item
library(zoo)
data$Item_Weight <- na.aggregate(data$Item_Weight,by=data$Item_Identifier,FUN=mean)
sum(is.na(data$Item_Weight)) # This equals 0, so we have replaced all missing values

# Let's take care of Outlet_Size
table(data$Outlet_Size,data$Outlet_Type)
# We can see that the outlet type does a good job telling us what outlet size category a store resides in
# Let's create a new data set with just NA's for Outlet_Size, and then assign the Outlet_Size based off of the Outlet_Type
# Then, we will bring this new data back to the original data frame
na.outlet_size <- data[is.na(data$Outlet_Size),]
nona.outlet_size <- data[!is.na(data$Outlet_Size),]
# Check to make sure filtering was correct
nrow(na.outlet_size)+nrow(nona.outlet_size)
nrow(data) # Good to go

na.outlet_size$Outlet_Size <- ifelse(na.outlet_size$Outlet_Type == "Grocery Store", "Small",
                                     ifelse(na.outlet_size$Outlet_Type == "Supermarket Type1", "Small", "Medium"))
table(na.outlet_size$Outlet_Size,na.outlet_size$Outlet_Type)
# It turns out that the only missing data for Outlet_Size is missing for factors of 'Grocery Store' and 'Supermarket Type1' of Outlet_Type

# Combine datasets back together
data <- rbind(na.outlet_size,nona.outlet_size)

# Look for any other missing data
library(mice)
md.pattern(data)
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# The only thing missing now is our Item_Outlet_Sales for our test data. Looks good!

## Feature engineering time!

# An item visibility of 0, does not make sense. Let's treat this as missing info
# We will replace them with the mean Item_Visibility for a particular product
# First, change 0 values to NA
data[data$Item_Visibility==0,c("Item_Visibility")] <- NA
data$Item_Visibility <- na.aggregate(data$Item_Visibility,data$Item_Identifier,FUN=mean)
sum(is.na(data$Item_Visibility)) # 0, good to go!

# Create Item_Type_Combined category for FD == Food, NC == Non-consumable, and DR == Drink
# I need to get the first two characters of id
data$Item_Type_Combined <- as.factor(substr(data$Item_Identifier,1,2))
levels(data$Item_Type_Combined)
# Create the new levels
levels(data$Item_Type_Combined) <- c(levels(data$Item_Type_Combined),"Drink","Food","Non-consumable")
data[data$Item_Type_Combined=="DR",c("Item_Type_Combined")] <- "Drink"
data[data$Item_Type_Combined=="FD",c("Item_Type_Combined")] <- "Food"
data[data$Item_Type_Combined=="NC",c("Item_Type_Combined")] <- "Non-consumable"
table(data$Item_Type_Combined) # All good!

# Determine the number of years that a store had been in operation. We do 2013, because this is sales data collected in 2013
data$Outlet_Years <- 2013 - data$Outlet_Establishment_Year

# Modify categories of Item_Fat_Content
data[data$Item_Fat_Content=="LF",c("Item_Fat_Content")] <- "Low Fat"
data[data$Item_Fat_Content=="low fat",c("Item_Fat_Content")] <- "Low Fat"
data[data$Item_Fat_Content=="reg",c("Item_Fat_Content")] <- "Regular"
# We also, need to create a new category of Non-Edible, because some items are non-consumables!
levels(data$Item_Fat_Content) <- c(levels(data$Item_Fat_Content),"Non-edible")
data[data$Item_Type_Combined=="Non-consumable","Item_Fat_Content"] <- "Non-edible"
table(data$Item_Fat_Content)
# Drop unused levels from factor
data$Item_Fat_Content <- droplevels(data$Item_Fat_Content)

## Model building time!
# resplit the data into train and test
train_data <- data[data$Source == "train",]
test_data <- data[data$Source == "test",]

# Check to see how the sales variable is distributed
library(ggplot2)
ggplot(train_data,aes(x=Item_Outlet_Sales)) +
  geom_histogram(bins = 70)

# Compare that to the log sales
ggplot(train_data,aes(x=log(Item_Outlet_Sales))) +
  geom_histogram(bins = 70)

# We can observe that the normal sales variable is right-skewed. Whereas the log sales has a more normal distribution.
# We will model both and compare results

# Linear regression model
lm.model <- lm(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type_Combined + Item_MRP  + 
                 Outlet_Years + Outlet_Size + Outlet_Location_Type, data = train_data)
summary(lm.model)
plot(lm.model)
# The above residual plots shows heteroskedasticity (big word... cool.)

test_data$lm.model.predictions <- predict(lm.model,newdata=test_data)

# When we run the above line, we obtain a warning message that says prediction from a rank-deficient may be misleading
# After looking into this error message, there may be some collinear covarites:
length(lm.model$coefficients) > lm.model$rank
# Check for collinearity
library(car)
vif(lm.model)
alias(lm.model)

# Forward selection
fwd.model <- step(lm.model,direction="forward")
summary(fwd.model) # All variables included as original
plot(fwd.model)
test_data$fwd.model.predictions <- predict(fwd.model,newdata=test_data)

# We get the same error message as the original model, since they are the same model.

# Backward selection
bwd.model <- step(lm.model,direction="backward")
summary(bwd.model) # Only 3 predictors included: Item_Fat_Content, Item_MRP, and Outlet_Identifier
plot(bwd.model)
test_data$bwd.model.predictions <- predict(bwd.model,newdata=test_data)

# Decision tree model
library(rpart)
bigmart.rpart <- rpart(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type_Combined + Item_MRP + Outlet_Identifier + 
                         Outlet_Years + Outlet_Type + Outlet_Size + Outlet_Location_Type,
                       data = train_data)
summary(bigmart.rpart)
plot(bigmart.rpart)
text(bigmart.rpart)
printcp(bigmart.rpart)
plotcp(bigmart.rpart)
test_data$tree.predictions <- predict(bigmart.rpart,newdata=test_data)

# Random forest model
# We must convert all varibles to class factor for the randomForest algorithm to work properly
str(train_data)
# Outlet_Size is a variable that is of character type.
train_data$Outlet_Size <- as.factor(train_data$Outlet_Size)

library(randomForest)
bigmart.rforest <- randomForest(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type_Combined + Item_MRP + Outlet_Identifier + 
                                  Outlet_Years + Outlet_Type + Outlet_Size + Outlet_Location_Type,
                                data = train_data,importance=TRUE)
summary(bigmart.rforest)

# Function to create a variable importance plot
rfimp = function(rffit) {barplot(sort(rffit$importance[,1]),horiz=F,
                                 xlab="Mean Decreasing in Accuracy",main="Variable Importance",las=3)
}
rfimp(bigmart.rforest)

# We have to set the Outlet_Size variable to factor
test_data$Outlet_Size <- as.factor(test_data$Outlet_Size)
test_data$rforest.predictions <- predict(bigmart.rforest,newdata=test_data)
  

# Let's check out a transformation on our outcome variable
library(ggplot2)
ggplot(train_data,aes(x=Item_Outlet_Sales)) + 
  geom_histogram(bins=150)
# Check out log distribution
ggplot(train_data,aes(x=log(Item_Outlet_Sales))) + 
  geom_histogram(bins=150)

## Let's do all modeling steps again with the log sales
# Create the new variable
train_data$log_Item_Outlet_Sales <- log(train_data$Item_Outlet_Sales)

# Linear regression model
log.lm.model <- lm(log_Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type_Combined + Item_MRP + Outlet_Identifier + 
                 Outlet_Years + Outlet_Type + Outlet_Size + Outlet_Location_Type, data = train_data)
summary(log.lm.model)
plot(log.lm.model)

test_data$log.lm.model.predictions <- exp(predict(lm.model,newdata=test_data))

# Forward selection
log.fwd.model <- step(log.lm.model,direction="forward")
summary(log.fwd.model) # All variables included as original
plot(log.fwd.model)
test_data$log.fwd.model.predictions <- exp(predict(log.fwd.model,newdata=test_data))

# Backward selection
log.bwd.model <- step(log.lm.model,direction="backward")
summary(log.bwd.model) # Only 3 predictors included: Item_Fat_Content, Item_MRP, and Outlet_Identifier
plot(log.bwd.model)
test_data$log.bwd.model.predictions <- exp(predict(log.bwd.model,newdata=test_data))

# Decision tree model
library(rpart)
log.bigmart.rpart <- rpart(log_Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type_Combined + Item_MRP + Outlet_Identifier + 
                         Outlet_Years + Outlet_Type + Outlet_Size + Outlet_Location_Type,
                       data = train_data)
summary(log.bigmart.rpart)
plot(log.bigmart.rpart)
text(log.bigmart.rpart)
printcp(log.bigmart.rpart)
plotcp(log.bigmart.rpart)
test_data$log.tree.predictions <- exp(predict(log.bigmart.rpart,newdata=test_data))

# Random forest model
log.bigmart.rforest <- randomForest(log_Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type_Combined + Item_MRP + Outlet_Identifier + 
                                  Outlet_Years + Outlet_Type + Outlet_Size + Outlet_Location_Type,
                                data = train_data,importance=TRUE)
summary(log.bigmart.rforest)

rfimp(log.bigmart.rforest)
test_data$log.rforest.predictions <- exp(predict(log.bigmart.rforest,newdata=test_data))

# Write predictions to their own separate .csv files
write.csv(test_data[,c("Item_Identifier","Outlet_Identifier","tree.predictions")],"~/R_Projects/bigmart_sales_prediction/tree_predictions.csv")
write.csv(test_data[,c("Item_Identifier","Outlet_Identifier","rforest.predictions")],"~/R_Projects/bigmart_sales_prediction/rforest_predictions.csv")
write.csv(test_data[,c("Item_Identifier","Outlet_Identifier","bwd.model.predictions")],"~/R_Projects/bigmart_sales_prediction/bwd_model_predictions.csv")
write.csv(test_data[,c("Item_Identifier","Outlet_Identifier","log.tree.predictions")],"~/R_Projects/bigmart_sales_prediction/log_tree_predictions.csv")
write.csv(test_data[,c("Item_Identifier","Outlet_Identifier","log.rforest.predictions")],"~/R_Projects/bigmart_sales_prediction/log_rforest_predictions.csv")
write.csv(test_data[,c("Item_Identifier","Outlet_Identifier","log.bwd.model.predictions")],"~/R_Projects/bigmart_sales_prediction/log_bwd_model_predictions.csv")





