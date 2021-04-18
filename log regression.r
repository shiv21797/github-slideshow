#remove all environment variables
rm(list = ls(all.names=T))
install.packages("usdm")#VIF
install.packages("ROCR")#ROC
install.packages("survey")
install.packages("caret")
install.packages("dummies")
?caret
library(usdm)
library(ROCR)
library(survey)
library(caret)
library(dummies)
setwd("C:/Users/SHIV/Downloads/")
Data <- read.csv(file ="Loan_Data_1.csv", header=TRUE, sep=",")
# selecting required attribute
names(Data)
#Data understanding
dim(Data)
str(Data)
summary(Data)

sum(is.na(Data))# unlike here check na for every column
#replace NA values by 0 # based on domain knowledge to replace NA with 0or blank or mean max min median mode
Data[is.na(Data)]<-0

Data=Data[,-8]
#remove na values if required 
#Data<-na.omit(Data)

##Exploratry analysis one col #do it for every columns

sd(x=Data$loan_amnt)
mean(x=Data$loan_amnt)
boxplot(x=Data$loan_amnt)
hist(x=Data$loan_amnt)
#cleaning
Data$revol_util<- sapply(Data$revol_util, function(x) sub("%","",x)) # applying for every var in revol_util and by applying ananomous fun subsituting % with blank
Data$revol_util<- as.numeric(Data$revol_util)
Data$Default<- as.factor(Data$Default)
Data$int_rate<- sapply(Data$int_rate, function(x) sub("%", "",x))
Data$int_rate<- as.numeric(Data$int_rate)
Data$grade<- as.factor(Data$grade)
#doing 1 hot coding for converting categorical var to numerical var and then remove any one column of that we have created
#here we doing explicitly but in logistic regression it will automatic convert cat var to num var
Data_1 <- dummy.data.frame(Data_1, names=c("grade"), sep="_")
Data_1 <- Data_1[,-10]

Data<-na.omit(Data)# remove 
str(Data)

#split the data into train and test set
rows<- seq(from=1,to= nrow(Data),by=1)
#take random sample for train set
train_rows <- sample(x=rows,size=(0.8*nrow(Data)))#no. of rows 
Train_Data<- Data[train_rows,]
Test_Data<- Data[-train_rows,]
#check for correltion and multicollinearity
#cor_data<- data [-c(1,4:6,10)]
cor_Matrix <-cor(Train_Data[-c(1,4)])
cor_Matrix
vif<-vif(x=Train_Data[-c(1,4)])
#building logistic regression

Log_Reg<- glm(formula = Default~., family = binomial, data=Train_Data)# glm and lm are from base library in environment it is by default loaded
summary(Log_Reg)
#Log_Reg<- glim(formula = Default~loan_amnt+revol_bal+...), we can see the var importance by building diff diff models for varables and for including and excluding var.s in model
#Wald test , for knowing var.is imp(significant) or not by p-value
regTermTest(Log_Reg, "loan_amnt")
regTermTest(Log_Reg,  "revol_bal")

#Relative var importance
varImp(Log_Reg)
# Prediction on Test data(for train data also that i have not write)
Test_set_prob <-predict(object=Log_Reg,newdata=Test_Data[,-1],type='response')#here -1 is for removing dependent var (y) for data
Test_pred_class <- factor(ifelse(test=Test_set_prob>0.4,yes = 1,no = 0))#changing in prob cutoff value chage the accuracy if we want 0,1 are equally imp than we check overall accuracy in some case 0 is more imp or 1 is more imp
table(Test_Data$Default,Test_pred_class)#for confusion matrix
#calculating accuracy, precision,and recall
#Build Confusion Matrix or classification matrix
confusionMatrix<-table(Test_Data$Default,Test_pred_class)
confusionMatrix
##accuracy
Accuracy <- sum(diag(confusionMatrix))/sum(confusionMatrix)*100
Accuracy
##precision
Precision<- confusionMatrix[2,2]/(confusionMatrix[2,2]+confusionMatrix[1,2])
Precision
##recall
Recall<- confusionMatrix[2,2]/(confusionMatrix[2,2]+confusionMatrix[2,1])
Recall
#False positive rate
FPR<-confusionMatrix[1,2]/(confusionMatrix[1,1]+confusionMatrix[1,2])
FPR
