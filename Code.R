## 1. Setting the working directory and clearing the R environment



rm(list=ls(all=T))
setwd("C:/Users/Punit/Desktop")



## 2. Loading the required libraries 



library(RColorBrewer)
library(rattle)
library(ipred)
library(ROSE)
library(ada)
library(rpart.plot)
library(rpart)
library(randomForest)
library(C50)
library(factoextra)
library(xgboost)
library(glmnet)
library(mice)
library(dplyr)
library(ROCR)
library(DMwR)
library(car)
library(MASS)
library(vegan)
library(dummies)
library(infotheo)
library(caTools)
library(caret)
library(e1071)
library(corrplot)
library(ggplot2)


## 3. Reading the data in R 

train= read.csv("Train.csv", sep= ",")
test= read.csv("Test.csv")

View(train)
View(test)


## 4. Data Understadning, Feature Engineering and Data Munging 


dim(train)
str(train)
train2 = train
sum(is.na(train))
test$Item_Outlet_Sales=0
combi= rbind(train,test)
View(combi)
colSums(is.na(combi))
# Total NAs per column  
str(combi)

combi=knnImputation(combi, k=5)
# Using kNN imputation to imppute the missing values. But, there still are values which are missing and not
# recorded as NAs

sum(is.na(combi))
# 0 as of now
substring("Punit",1,3)

combi$Item_Identifier=lapply(combi$Item_Identifier, function(x){substring(x,1,2)})
# Substringing the item id to extract only the first 2 alphabets

class(combi$Item_Identifier)
combi$Item_Identifier= as.factor(as.character(combi$Item_Identifier))
table(combi$Item_Identifier)
# Checking the distribution of the item id

nc= which(combi$Item_Identifier=="NC")
nc[1:5]
for(i in nc){
  combi[i,3]="None"
}
# All the non consumables cannot have a fat level. Hence imputing a 'none' against that 

sum(is.na(combi$Item_Fat_Content))
length(nc)
class(combi$Item_Fat_Content)
combi$Item_Fat_Content= as.factor(combi$Item_Fat_Content)
# Converting the Fat Content to a factor variable 

table(combi$Item_Fat_Content)
# Checking their distribution 

table(combi$Outlet_Size)
table(combi$Outlet_Identifier)
# Checking the distributions of outlet size and outlet ID  

grocery= subset(combi, Outlet_Type=="Grocery Store")
View(grocery)
table(grocery$Outlet_Size=="", grocery$Outlet_Identifier=="OUT010")
dim(grocery)
combi$Outlet_Size[combi$Outlet_Identifier=="OUT010"]="Small"
spmtype1= subset(combi, Outlet_Type=="Supermarket Type1")
dim(spmtype1)
# Checking a few distributions. Trying to impute the missing values in the outlet size variable 

View(spmtype1)
table(spmtype1$Outlet_Location_Type=="Tier 1", spmtype1$Outlet_Size)
# tier 1 is definitely not high

table(spmtype1$Outlet_Location_Type=="Tier 2", spmtype1$Outlet_Size)
table(spmtype1$Outlet_Location_Type=="Tier 3", spmtype1$Outlet_Size)
table(combi$Outlet_Location_Type=="Tier 1", combi$Outlet_Size)
table(combi$Outlet_Location_Type=="Tier 2", combi$Outlet_Size)
table(combi$Outlet_Location_Type=="Tier 3", combi$Outlet_Size)
# I finally have decided to make the imputations. 


combi$Outlet_Size[combi$Outlet_Identifier=="OUT017"]="Small"
combi$Outlet_Size[combi$Outlet_Identifier=="OUT045"]="Small"
table(combi$Outlet_Size)
table(combi$Item_Fat_Content)
# imputed the missing values with a "Small" size
 
combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"]="Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="low fat"]="Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"]="Regular"
combi$Item_Fat_Content=as.factor(as.character(combi$Item_Fat_Content))
# The fat content has only 2 levels but with different spellings. Making it more uniform 

table(combi$Item_Fat_Content)
# Looks good 

str(combi)
combi2=combi
a=tapply(combi2$Item_Visibility, combi2$Outlet_Identifier, function(x){sum(x)})
a
# We observe that the sum of visibilty is not equa to 100%. This is logically incorrect
b=100-a
b
sum(b)
c=tapply(combi2$Item_Visibility, combi2$Outlet_Identifier, function(x){sum(x==0.000000000)})
c
b/c
sum(a)
check=which(combi2$Item_Visibility==0.000000000)
combi$Item_Visibility[check]=mean(b/c)
1000-sum(a)
# Having 0.0000 visibility does not make sense. The item would not be in the shop then
# Thus, imputing these values with the average percentage of missing visibility values. 

combi$Item_Visibility= combi2$Item_Visibility

combi$Outlet_Establishment_Year= 2013-combi$Outlet_Establishment_Year
# Munging the data for better extraction of information 

combi$Item_Outlet_Sales = combi$Item_Outlet_Sales/combi$Item_MRP
# The Item sale is directly proportional to its MRP. It would be more logical to predict the sale of 
# units. Thus, dividing total sale by the MRP of the product 

table(combi$Outlet_Identifier)
str(combi)

summary(combi)
dim(combi)
combi2$Item_MRP=ifelse(combi2$Item_MRP>31 & combi2$Item_MRP<=94.02,"Low",
                       ifelse(combi2$Item_MRP>94.02 & combi2$Item_MRP<= 142.26, "Medium",
                              ifelse(combi2$Item_MRP>142.26 & combi2$Item_MRP<=185.87,"High","VeryHigh"))) 
# Binning the item MRP
combi2$Item_Weight=ifelse(combi2$Item_Weight>4.55 & combi2$Item_Weight<=9.195,"Low",
                       ifelse(combi2$Item_Weight>9.195 & combi2$Item_Weight<= 12.688, "Medium",
                              ifelse(combi2$Item_Weight>12.688 & combi2$Item_Weight<=16.250,"High","VeryHigh"))) 
# Binning the item Weight 
str(combi2)
combi$Item_Weight= combi2$Item_Weight
combi2$Item_Weight= as.factor(combi2$Item_Weight)
sum(table(combi2$Item_MRP))
combi$Item_MRP= combi2$Item_MRP
str(combi)
combi$Item_MRP= as.factor(combi$Item_MRP)
weight= combi$Item_Weight
dim(train)
combi$Item_Outlet_Sales= combi2$Item_Outlet_Sales

combi=combi[,-6]
# Removing the item MRP column 

## 5. Making the data ready for using in models

ctrain= combi[1:8523,]
ctest= combi[-c(1:8523),]
# Retrieving the original data
dim(ctrain)
dim(ctest)
ctest$Item_Outlet_Sales= NULL
# Ready-ing the data 

## 6. Linear Regression models


sum(is.na(combi))

View(ctrain)
model= lm(Item_Outlet_Sales~., data=ctrain)
summary(model)
pred= predict(model, newdata=ctest)
mean(pred)
pred= pred*test$Item_MRP
length(temrp)
vif(model)

martt=read.csv("Test.csv")
sub1= data.frame("Item_Identifier"=test$Item_Identifier, "Outlet_Identifier"=test$Outlet_Identifier,"Item_Outlet_Sales"= pred)
?write.csv
write.csv(sub1,"sub1.csv")
dim(sub1)
# Rank 209  
model2= stepAIC(model)
summary(model2)
pred2= predict(model2, newdata=ctest) 
pred2= pred2*test$Item_MRP
sub2= data.frame("Item_Identifier"=test$Item_Identifier, "Outlet_Identifier"=test$Outlet_Identifier,"Item_Outlet_Sales"= pred2)
write.csv(sub2,"sub2.csv")
# rank 123
par(mfrow=c(2,2))
plot(model2)


# rank 515 with interaction degree 2 
mart4$Item_Outlet_Sales= log(mart4$Item_Outlet_Sales)
mean(mart4$Item_Outlet_Sales)
ctrainN= ctrain[-c(6410,5764),]

ctrain$Item_Outlet_Sales= exp(ctrain$Item_Outlet_Sales)
mean(ctrain$Item_Outlet_Sales)
model3=lm(Item_Outlet_Sales~.^3, data=ctrain)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
pred3= predict(model3,newdata=ctest)
mean(pred3)
pred3=exp(pred3)
pred3= pred3*temrp
pred3=exp(pred3)
sub3= data.frame("Item_Identifier"=martt$Item_Identifier, "Outlet_Identifier"=martt$Outlet_Identifier,"Item_Outlet_Sales"= pred3)
write.csv(sub3, "sub3.csv")
aa=boxcox(model,lambda= seq(0,1,0.1))
aa$x
aa$y
summary(model)
par(mfrow=c(2,2))
plot(model)


model4= stepAIC(model3)
summary(model3)
pred4= predict(model4, newdata=martt)
sub3= data.frame("Item_Identifier"=martt$Item_Identifier, "Outlet_Identifier"=martt$Outlet_Identifier,"Item_Outlet_Sales"= pred4)
write.csv(sub4, "sub4.csv")
dim(mart)
dim(martt)
martt= martt[,-c(1)]
mart3= rbind(mart[,1:10],martt)
View(mart3)
str(mart3)
summary(mart3)


install.packages("FactoMineR")
library(FactoMineR)
library(vegan)
library(infotheo)

View(ctrain)
str(ctrain)
dim(ctrain)
ctrain3= apply(ctrain3, 2, as.character)
str(ctrain3)
ctrain3=ctrain[,-c(12)]
View(ctrain3)
trial= MCA(ctrain3)
summary(trial)
?MCA
View(trial$call)
trial
View(trial$ind)
trial$eig
trial$var$coord


# rank 515 with interaction degree 2 
mart4$Item_Outlet_Sales= log(mart4$Item_Outlet_Sales)
mean(mart4$Item_Outlet_Sales)
ctrainN= ctrain[-c(6410,5764),]

ctrain$Item_Outlet_Sales= exp(ctrain$Item_Outlet_Sales)
mean(ctrain$Item_Outlet_Sales)
nmodel3=lm(Item_Outlet_Sales~.^2, data=ctrain2)
summary(nmodel3)
par(mfrow=c(2,2))
plot(model3)
pred3= predict(nmodel3,newdata=ctest2)
mean(pred3)
pred3=exp(pred3)
pred3= pred3*temrp
pred3=exp(pred3)
sub3= data.frame("Item_Identifier"=martt$Item_Identifier, "Outlet_Identifier"=martt$Outlet_Identifier,"Item_Outlet_Sales"= pred3)
write.csv(sub3, "sub3.csv")
aa=boxcox(model,lambda= seq(0,1,0.1))
aa$x
aa$y
summary(model)
par(mfrow=c(2,2))
plot(model)

nmodel4= stepAIC(nmodel3)
model4= stepAIC(model3)
summary(nmodel4)
pred4= predict(nmodel4, newdata=ctest2)
pred4=pred4*temrp
mean(pred4)
sub4= data.frame("Item_Identifier"=martt$Item_Identifier, "Outlet_Identifier"=martt$Outlet_Identifier,"Item_Outlet_Sales"= pred4)
write.csv(sub4, "sub4.csv")
dim(mart)
dim(martt)
martt= martt[,-c(1)]
mart3= rbind(mart[,1:10],martt)
View(mart3)
str(mart3)
summary(mart3)

?

sum_func= function(x=0,y=0,z=0){
  sum=x+y+z
  print(sum)
  cat("Max is", max(x,y,z),"\n")
  cat("Min is", min(x,y,z),"\n")
}

sum_func(1000,2222,3333)


## 7. Random Forest 

model_rf = randomForest(Item_Outlet_Sales ~ . , ctrain, ntree = 500,mtry = 3)
summary(model_rf)
plot(model_rf)
varImpPlot(model_rf)
par(mfrow=c(1,1))
rf_train_pred = predict(model_rf)
preds_rf_test <- predict(model_rf, ctest)
preds_rf_test= preds_rf_test*test$Item_MRP
rfsub= data.frame("Item_Identifier"=test$Item_Identifier, "Outlet_Identifier"=ctest$Outlet_Identifier,"Item_Outlet_Sales"= preds_rf_test)
write.csv(rfsub, "rfsub.csv")
dim(ctest)
length(preds_rf_test)
dim(rfsub)


## 8. XgBoost


dim(ctrain)
dim(ctest)
View(ctest)
comb= rbind(ctrain,ctest)
str(comb)
itemid= dummy(comb$Item_Identifier)
itemfat= dummy(comb$Item_Fat_Content)
itemtype= dummy(comb$Item_Type)
outid= dummy(comb$Outlet_Identifier)
outsize= dummy(comb$Outlet_Size)
outloc= dummy(comb$Outlet_Location_Type)
outtype= dummy(comb$Outlet_Type)
comb= comb[,-c(1,3,5,6,8,9,10)]
comb= cbind(comb,itemid,itemfat,itemtype,outid,outsize,outloc, outtype)
dim(comb)
dim(train)
cctrain= comb[1:8523,]
cctest= comb[8524:14204,]

xgb.ctrl <- trainControl(method = "cv", number = 5,
                         search='random')

set.seed(123)
xgb.tune <-train(Item_Outlet_Sales~.,
                 data = cctrain,
                 method="xgbLinear",
                 trControl=xgb.ctrl,
                 tuneLength=40)

print(xgb.tune)
plot(xgb.tune)



XgB_final= predict(xgb.tune, newdata= cctest)
length(XgB_final)
XgB_final= XgB_final*test$Item_MRP
xgbsub= data.frame("Item_Identifier"=test$Item_Identifier, "Outlet_Identifier"=ctest$Outlet_Identifier,"Item_Outlet_Sales"= XgB_final)
write.csv(xgbsub, "xgbsub.csv")
mean(XgB_final)

