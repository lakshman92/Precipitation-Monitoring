#Script water
#Introduction to Data Analytics
#Week 6 Group Assignment
#Author: Lakshman Arunachalam and Thomas Allen

#Step-1 Read data into variable wa
wa <- read.csv("water.csv")

names(wa)

#Step-2 Setting seed
library(caret)
set.seed(2015)


#Step-3 Create partition Data
partition = createDataPartition(wa$BSAAM, p=0.7, list=FALSE)
train = wa[partition, ]
test = wa[-partition, ]

#Step-4 Regression model-1

wa.model_1 <- lm(BSAAM ~ ., data= train[,-1])
summary(wa.model_1)


#Step-5 Removing the weakest varibale APSAB not satisfying the p value and creating model-2

wa.model_2 <- lm(BSAAM ~ APMAM+APSLAKE+OPBPC+OPRC+OPSLAKE, data=train[,-1])
summary(wa.model_2)

#Step-6 Remove the next weakest variable OPBPC not satisfying the p value.

wa.model_3 <- lm(BSAAM ~ APMAM+APSLAKE+OPRC+OPSLAKE, data=train[,-1])
summary(wa.model_3)

#Step-7 Remove the next weakest variable APSLAKE not satisfying the p value

wa.model_4 <- lm(BSAAM ~ APMAM+OPRC+OPSLAKE, data=train[,-1])
summary(wa.model_4)

#Step-8 Remove the next weakest variable OPRC not satisfying the p value

wa.model_5 <- lm(BSAAM ~ APMAM+OPSLAKE, data=train[,-1])
summary(wa.model_5)

#Model 4 is the best model as it explains 91% of the variation with p value < 0.005

#step-9 With the created model, testing the test data and computing RSME

pred.test <- predict(wa.model_4, test[,-1])
rmse <- sqrt(mean((test$BSAAM - pred.test)^2))
rmse

mean(wa$BSAAM)

#The liniear Equation
#BSAAM = 16223.1 + 1321.2*APMAC + 1636.9*OPRC + 2456.8*OPSLAKE


