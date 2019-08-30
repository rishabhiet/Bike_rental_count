rm(list=ls(all=T))
setwd("C:/Users/risha/Desktop/BIKE rental project/Bike_rental")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
trans = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Explore the data##########################################

#gives variable names
str(trans)

#gives number of variable and observation
dim(trans)

#FOUR METHOD USED IN PREPROCESSING ARE. 1) MISSING VALUE ANALYSIS 2) OUTLIER ANALYSIS 3) DATA SELECTION 4) DATA SCALING

##################################Missing Values Analysis###############################################

missing_val = data.frame(apply(trans,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(trans)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Train)") + theme_bw()
#DATA DOES NOT HAVE ANY MISSING VALUE SO WE ARE NOT USE MISSING VALUE IMPUTATION

############################################Outlier Analysis#############################################
#OUTLIER ANALYSIS
#BY USING TWO WAYS OUTLIER CAN BE HANDLE.
#BY REMOVING THE OUTLIER VALUES.
#OR BY PUTTING OUTLIER AS NULL VALUE AND THE IMPUTE USING KNN IMPUTATION.
#SO WE ARE USING FIRST METHOD AS IS GIVES LESSER ERROR

#selecting only numeric
numeric_index = sapply(trans,is.numeric) 
numeric_data = trans[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(trans))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",cnames[i])))
}
# # #Remove outliers using boxplot method
val = trans$previous[trans$previous %in% boxplot.stats(trans$previous)$out]
# 
trans = trans[which(!trans$previous %in% val),]

# # #loop to remove from all variables
for(i in cnames){   print(i)
  val = trans[,i][trans[,i] %in% boxplot.stats(trans[,i])$out]
  #print(length(val))
  trans = trans[which(!trans[,i] %in% val),]
}
#AFTER REMOVING OUTLIER WE GET 676 OBSERVATION FROM 731 OBSERVATIONS SO 55 OBSERVATION EXIST AS AN OUTLIER IN TRANS DATASET¶

##################################Feature Selection################################################
## Correlation Plot

corrgram(marketing_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(marketing_train,is.factor)
factor_data = marketing_train[,factor_index]

for (i in 1:7)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$responded,factor_data[,i])))
}
## Dimension Reduction
marketing_train = subset(trans, 
                         select = -c(atemp))

##################################Feature Scaling################################################
#Normality check
qqnorm(trans$temp)
hist(trans$temp)

#Normalisation
cnames = c("temp","atemp","hum","windspeed","casual","registered","cnt")

for(i in cnames){
  print(i)
  trans[,i] = (trans[,i] - min(trans[,i]))/
    (max(trans[,i] - min(trans[,i])))
}
###################################Model Development#######################################
#Clean the environment
rmExcept("trans")

#Divide the data into train and test
set.seed(123)
train_index = sample(2:nrow(trans), 0.8 * nrow(trans))
train = trans[train_index,]
test = trans[-train_index,]

##Decision tree for Regression
#Develop Model on training data
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-14])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,14], predictions_DT)

# MAPE for Decision tree = 7.285%

###Random Forest
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)

#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-14])

#MAPE
#calculate MAPE
MAPE(test[,14], RF_Predictions)

# MAPE for Random Forest = 1.188%

#Linear Regression
#check multicollearity
library(usdm)
vif(train[,-14])

vifcor(train[,-14], th = 0.9)

#run regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,14])

#Calculate MAPE
MAPE(test[,14], predictions_LR)

# MAPE for Linear Regression = 1.32951e-13

##KNN Implementation
library(class)

#Predict test data
KNN_Predictions = knn(train[, 2:14], test[, 14], train$cnt, k = 7)

#Calculate MAPE
MAPE(test[,14], KNN_Predictions)

# MAPE for KNN Regression = 7.2257%

################################SUMMARY#########################################

# DECISION TREE
# MAE for Decision tree = 0.07285
# MAPE for Decision tree = 7.285%
# MSE for Decision tree = 0.00713
# RMSE for Decision tree = 0.0844

# RANDOM FOREST
# MAE for Random Forest = 0.01188
# MAPE for Random Forest = 1.188%
# MSE for Random Forest = 0.0002834
# RMSE for Random Forest = 0.01683

# LINEAR REGRESSION
# MAE for Linear Regression = 1.32951e-15
# MAPE for Linear Regression = 1.32951e-13
# MSE for Linear Regression = 2.59827e-30
# RMSE for Linear Regression = 1.61191e-15

# KNN K NEAREST NEIGHBOUR
# MAE for KNN Regression = 0.07225
# MAPE for KNN Regression = 7.2257%
# MSE for KNN Regression = 0.009388
# RMSE for KNN Regression = 0.09689



















