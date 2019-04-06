library(readxl)
# setting seed
set.seed(12345)
# Importing data
data=read_xlsx("spambase.xlsx")

# deviding data in test and train 
n  <- dim(data)[1]
id <- sample(1:n,floor(0.5*n))
train <- data[id,]
test  <- data[-id,]


# Appling Logistic Regeression 
logistic <- glm(Spam~.,data = train,family = "binomial")
summary(logistic)
prediction <- predict(logistic,train,type="response")

# Misclassification rate confucsion Matrix based on Y=1 if Predected Y>0.5,else = 0 
pred1 <- ifelse(prediction>0.5,1,0)
tab <- table(Prediction=pred1,Actual=train$Spam)
Missclassification = (tab[1,2]+tab[2,1])/sum(tab)

# Misclassification rate confucsion Matrix based on Y=1 if Predected Y>0.9,else = 0 
pred2 <- ifelse(prediction>0.9,1,0)
tab2 <- table(Prediction=pred2,Actual=train$Spam)
misclassification = (tab2[1,2]+tab2[2,1])/sum(tab2)


# misclasification resulted from KKNN 
library(kknn)
kknn <- kknn(Spam~.,train=train,test=test,k=30)
summary(kknn)

# Misclassification rate confucsion Matrix based on Y=1 if Predected Y>0.5,else = 0 
predictkn <- ifelse(kknn$fitted.values>0.5,1,0)
tab3 <- table(Predicted=predictkn,Actual=test$Spam)
kknnmisclassification = (tab3[1,2]+tab3[2,1])/sum(tab3)

# Misclassification rate confucsion Matrix based on Y=1 if Predected Y>0.9,else = 0 

predictknn <- ifelse(kknn$fitted.values>0.9,1,0)
tab4 <- table(Predicted=predictknn,Actual=test$Spam)
kknnmisclassification0.9 = (tab4[1,2]+tab4[2,1])/sum(tab4)


