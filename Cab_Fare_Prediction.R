rm(list = ls())

setwd("D:/Data Science/Project1")
getwd()


#Load Libraries
x = c("ggplot2","dplyr","lubridate","tidyverse", "geosphere", "corrgram", "DMwR",  "randomForest", "unbalanced",  "e1071",
      "MASS", "rpart", "gbm", 'DataCombine', 'inTrees','caret','C50')
lapply(x, require, character.only = TRUE)
rm(x)

# load the data
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
summary(train)


# converting into proper datatype
train$fare_amount=as.numeric(as.character(train$fare_amount))
train$passenger_count=as.integer(train$passenger_count)
summary(train)

# filtering out the rows where pickup_longitude = dropoff_longitude and pick_up latitude = drop_off latitude
train = filter(train,
               train$pickup_longitude!= train$dropoff_longitude &
               train$pickup_latitude!=train$dropoff_latitude
)

#replace all "0" with NA
train[train==0]= NA
summary(train)


# fare_amount is target variable, so remove NA by filtering out all fare_amount>0, this will also filter out NA if available
train = filter(train,
               fare_amount>0&
               fare_amount<60
)


#missing Value

missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Train)") 




#passenger_count is categorical, so replace it by mode 
#function to calculate mode
mode= function(x){
  y=unique(x)
  as.numeric(as.character(y[which.max(tabulate(match(x,y)))]))
  
}

#impute with the mode
train$passenger_count[is.na(train$passenger_count)] = mode(train$passenger_count)
train$passenger_count = as.integer(train$passenger_count)

#Method
#Actual train$pickup_latitude[48]= 40.77205
#Mean =train$pickup_latitude[48]= 40.71222
#Median =train$pickup_latitude[48]= 40.75332 
#KNN = train$pickup_latitude[48]= 40.77271


#KNN imputation
train = knnImputation(train, k = 5)

# Outlier Analysis
train1 = train
summary(train)


# ## BoxPlots - Distribution and Outlier Check


cnames = colnames(train[,c(3:7)])
# 
for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount", group = 1), data = train)+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Num")+
            ggtitle(paste("Box plot of fare_amount for",cnames[i])))
 }
# 
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,gn5, ncol=3 )

# # #Remove outliers using boxplot method
train_data = train
#train = train_data

# replacing outlier with NA
for(i in cnames){
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  #print(length(val))
  train[,i][train[,i] %in% val] = NA
  }



#KNN imputation
train = knnImputation(train, k = 5)


###################
train2 = train
#train = train2

# library(geosphere)
#creating a distance column using distHaversine function
train$distance = distHaversine(train[,c(3,4)],train[,c(5,6)])
train$distance = as.numeric(train$distance)/1000

#which(is.na(train$pickup_datetime))
# removing a row which has pickup_datatime value not in proper format, pickup_datetime=43
train = train[-c(1265),]

# creating aditional column from datetime to year, month,day,dayOfWeek,hour,partOfDay 
train <- mutate(train,
                     pickup_datetime = ymd_hms(pickup_datetime),
                     month = as.factor(month(pickup_datetime)),
                     year = as.numeric(year(pickup_datetime)),
                     day = day(pickup_datetime),
                     dayOfWeek = as.factor(wday(pickup_datetime)),
                     hour = hour(pickup_datetime),
                     partOfDay = as.factor(round(hour * 2 / 10)),
                     hour = as.factor(hour(pickup_datetime))
                     
)
#converting into proper datatype
train$passenger_count = as.integer(train$passenger_count)


## Correlation Plot 
  corrgram(train[,-c(7)], order = F,
                         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot"
)

## drop unwanted columns
train <- train[,-c(2,9,11,12,14)]


qqnorm(train$fare_amount)

hist(train$fare_amount)

train3 = train
#Clean the environment
rmExcept("train")

train3 = train


#Divide data into train and test using stratified sampling method
set.seed(1234)

train.index = createDataPartition(train$fare_amount, p = .80, list = FALSE)
train = train[ train.index,]
test  = train[-train.index,]


# Define Mape
MAPE = function(x, y){
  mean(abs((x - y)/x*100))
}

#Decision Tree
fit = rpart(fare_amount ~. , data = train, method = "anova", minsplit=10)
summary(fit)
predictions_DT = predict(fit, test[,-1])
MAPE(test[,1], predictions_DT)
#41.88119

#Linear Regression
lm_model = lm(fare_amount ~. , data = train)
summary(lm_model)
predictions_LR = predict(lm_model, test[,-1])
MAPE(test[,1], predictions_LR)
#38.87821




##KNN Implementation
library(class)
#Predict test data
KNN_Predictions = knn(train[, 2:9], test[, 2:9], train$fare_amount, k = 5)
#convert the values into numeric
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))
#Calculate MAPE
MAPE(test[,1], KNN_Predictions)
#33.02988

#Random Forest
RF_model = randomForest(fare_amount ~.  , train, importance = TRUE, ntree=200, mtry=2)
RF_Predictions = predict(RF_model, test[,-1])
MAPE(test[,1], RF_Predictions)
importance(RF_model, type = 1)
#18.18387

###############################################################################

# Loading the test data
test_cab = read.csv("test.csv", header = T)

# filtering out the rows where pickup_longitude = dropoff_longitude and pick_up latitude = drop_off latitude
test_cab = filter(test_cab,
               test_cab$pickup_longitude!= test_cab$dropoff_longitude &
               test_cab$pickup_latitude!=test_cab$dropoff_latitude
)


#creating a distance column using distHaversine function
test_cab$distance = distHaversine(test_cab[,c(2,3)],test_cab[,c(4,5)])
test_cab$distance = as.numeric(test_cab$distance)/1000


# creating aditional column from datetime to year,hour 

test_cab <- mutate(test_cab,
                pickup_datetime = ymd_hms(pickup_datetime),
                
                year = as.numeric(year(pickup_datetime)),
                
                hour = hour(pickup_datetime),
                
                hour = as.factor(hour(pickup_datetime))
                
)


# droping out unwanted columns
test_cab = test_cab[,-c(1)]
#Random Forest
RF_model = randomForest(fare_amount ~.  , train, importance = TRUE, ntree=200, mtry=2)
test_cab$fare_amount = predict(RF_model, test_cab)
#write.csv(test_cab,"final_test_R.csv",row.names = F)

