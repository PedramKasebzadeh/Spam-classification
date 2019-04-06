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
misclassification = (tab2[1,2]+tab2[2,1])/sum(tab)
