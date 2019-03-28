getwd()
setwd("C:/Users/Sagar/Desktop/Interview/Homework 2")

Trainingdata<-read.csv("house_train.csv")
head(Trainingdata) #Loading the Training Data
summary(Trainingdata)

datafile_Test<-read.csv("house_test.csv") #Loading the Testing Data
head(datafile_Test)

#Method: Try to create a model with State variable as the explanatory variable
datamodel<-lm(price2013~state,data=Trainingdata)
summary(datamodel)
coef(datamodel) #Checking the coefficients

Intercept<-datamodel$coefficients[1]  #Finding the intercept
Intercept #By checking the summary of our regression model we get to know that our intercept is 281730

#Finding the states with the most expensive and least expensive average homes based on our regression ceofficients
data1<-coef(datamodel)
data1 #Putting the coefficients variable data1

Data2<-data.frame(data1)
colnames(Data2)[1]<-"Regression Values"
Data2 #As data1 is a vector, we put it in a dataframe form

datawithoutintercept<-Data2[-1, , drop = FALSE]
datawithoutintercept #As we know that data2 has only one column and if you try to manipulate it, it will turn into vector. Since we need to remove the intercept row, the way we can remove without still keeping it as a dataframe is by using drop=FALSE

D1<-which.max(datawithoutintercept$`Regression Values`)
D1
Max.State<-datawithoutintercept[7L,,drop=FALSE]
Max.State 

D2<-which.min(datawithoutintercept$`Regression Values`)
D2
Min.State<-datawithoutintercept[44L,,drop=FALSE]   
Min.State 


#Let's find the average price of homes in those states That is we need to find the average of homes in DC and WV
#The average price of home in DC

p<-data.frame(state="DC")
p
PredictDC<-predict(datamodel,p)
PredictDC

s<-data.frame(state="WV")
s
PredictWV<-predict(datamodel,s)
PredictWV

#we come up with a regressor that best predicts average home values in the test dataset. So we will be building a model in Train_dataset and then we will be applying that model to the test dataset to predict price2013 homes
#To check the accuracy of our model we use R-squared that runs from 0 to 100% So the approach is that we come up with models and check the Multiple R-squared,Adjusted R squared. A value near 100% is good model for us to use

#1st Method:Trying with price 2007
First_Pred<-lm(price2013~price2007,data=Trainingdata)
summary(First_Pred) #Multiple R-squared: 0.9092 Residual standard error: 65600 If you check the Pr(>|t|) column of price2007, we get to know it plays a significant role in predicting price value for year2013

#2nd Method: Trying with price2007 and poverty
Second_pred<-lm(price2013~poverty+price2007,data=Trainingdata)
summary(Second_pred) #Multiple R-squared: 0.9095 Residual standard error: 65470 If you check the Pr(>|t|) column of price2007, we get to know it plays a significant role in predicting price value for year2013

#3rd Method: Trying with poverty,price2007 and State
Third_pred<-lm(price2013~poverty+price2007+state,data=Trainingdata)
summary(Third_pred) #Multiple R squared:0.9354 Adjusted R squared:0.9351 Residual standard error: 65470
#If we check our Multiple R squared is increasing

#4th Method: Including county data along with other predictors
Forth_pred<-lm(price2013~poverty+price2007+county+state ,data=Trainingdata)
summary(Forth_pred) #Multiple R squared:0.9625 Adjusted R squared:0.9594 Residual standard error: 43830

#Trying an alternate method
Alter_pred<-lm(price2013~poverty+price2007+state+county + 0 ,data=Trainingdata)
summary(Alter_pred) #Removing the intercept from the coefficients
#The multiple R-squared is 98%. So we use this model to predict price 2013

#Now if try to use this model directly on our test environment, we will get an error
#The reason we will get an error because the list of counties in the training set is different (has fewer values) than the list of counties in the test set (For example grafton county is in test but not in train)


#To deal with this issue we use the Union Function
Alter_pred$xlevels[["county"]]<-union(Alter_pred$xlevels[["county"]], levels(datafile_Test$county))

#Predicting the data
datafile_Test$predicted2013s<-predict(Alter_pred,datafile_Test)


head(datafile_Test) #Got the predicted values

pred2013tests<-data.frame(datafile_Test$id,datafile_Test$predicted2013s)
colnames(pred2013tests)<-c("id","prediction")
pred2013tests #creating another column having only the id and the predicted values and create a CSV of that dataframe

write.csv(pred2013tests,"firstpreds.csv",row.names=FALSE)
