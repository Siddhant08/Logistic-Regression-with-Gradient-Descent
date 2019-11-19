#load data set
library(ISLR)
fix(Auto)
attach(Auto)

#create target variable
High <- ifelse(mpg<=22,0,1)

#merge new target variable to Auto
Auto <- data.frame(Auto,High)

#creating n-1 dummy variables for origin
#use the below commented line to install this package
#install.packages("dummies")
library(dummies)

Auto <- cbind(Auto, dummy(origin, sep = "Origin_"))
#drop existing mpg column from Auto
Auto = subset(Auto, select = c(horsepower,weight,year,AutoOrigin_1,AutoOrigin_2,High))


#split into test and training set
trainSize <- dim(Auto)[1] * 0.50
trainData <- sample(nrow(Auto), size = trainSize)

train <- Auto[trainData,]
test <- Auto[-trainData,]

#normalize the values except for target variable
standardizedTrain.X <- scale(train[,-6])
standardizedTest.X <- scale(test[,-6])

train.Y <- train[,6]
test.Y <- test[,6]


#############################################
#ex-4
#logistic regression with gradient descent

#sigmoid function
sigmoid <- function(z){1/(1+exp(-z))}

#Cost Function for gradient descent(this has to be minimized) using negative log-likelihood
cost <- function(beta)
{
  m <- nrow(standardizedTrain.X)
  g <- sigmoid(standardizedTrain.X%*%beta)
  J <- (1/m)*sum((-train.Y*log(g)) - ((1-train.Y)*log(1-g)))
  return(J)
}

#Define learning rate and iteration limit
alpha <- 0.1
num_iters <- 100

#initialize coefficients in the range [-0.7,0.7]
beta <- matrix(runif(5, -0.7, 0.7),nrow=5)

#Cost at inital beta(random values in range [-0.7,0.7])
cost(beta)

#gradient descent
for (i in 1:num_iters) {
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta <- beta - (alpha*delta)
}

#get predictions on test set
pred=list()
for (i in 1:length(test.Y)) {
  pred[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta)>=0.50) 1 else 0
}

#check model performance through error rate
mean(pred!=test.Y)

#confusion matrix
predVec <- unlist(pred)
table(test.Y,predVec)

#get test MSE
testMSE <- sum((test.Y - predVec)^2)/length(test.Y)

#get predictions on training set
pred=list()
for (i in 1:length(train.Y)) {
  pred[i] <- if(sigmoid(t(c(standardizedTrain.X[i],standardizedTrain.X[nrow(standardizedTrain.X)+i],standardizedTrain.X[nrow(standardizedTrain.X)*2+i],standardizedTrain.X[nrow(standardizedTrain.X)*3+i],standardizedTrain.X[nrow(standardizedTrain.X)*4+i]))%*%beta)>=0.50) 1 else 0
}

#check model performance through error rate
mean(pred!=train.Y)

#confusion matrix
predVec <- unlist(pred)
table(train.Y,predVec)

#get train MSE
trainMSE <- sum((train.Y - predVec)^2)/length(train.Y)

#ex-6 with approach A(as mentioned in report)
alpha <- 0.1
num_iters <- 100  #as specified in the question

#list to store test MSE in every iteration
testMSE_iter=vector()

#gradient descent
for (j in 1:num_iters) {
  #initialize coefficients in the range [-0.7,0.7]
  beta <- matrix(runif(5, -0.7, 0.7),nrow=5)
  
  #get predictions on test set
  pred=vector()
  
  for (i in 1:length(test.Y)) {
    pred[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta)>=0.50) 1 else 0
    
  }
  testMSE_iter[j] <- sum((test.Y - pred)^2)/length(test.Y)
  
}

#testMSE_iter has all the MSE values for learning rate = 0.1
#and total iterations = 100 for every iteration
boxplot(testMSE_iter,main="Test MSE over 100 iterations", ylab="MSE")

#ex-6 with approach B(as mentioned in the report)
alpha <- 0.1
num_iters <- 100  #as specified in the question

#list to store test MSE in every iteration
testMSE_iter=vector()

#initialize coefficients in the range [-0.7,0.7]
beta <- matrix(runif(5, -0.7, 0.7),nrow=5)

#gradient descent
for (j in 1:num_iters) {
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta <- beta - (alpha*delta)
  
  #get predictions on test set
  pred=vector()
  
  for (i in 1:length(test.Y)) {
    pred[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta)>=0.50) 1 else 0
    
  }
  testMSE_iter[j] <- sum((test.Y - pred)^2)/length(test.Y)
  
}

#testMSE_iter has all the MSE values for learning rate = 0.1
#and total iterations = 100 for every iteration
boxplot(testMSE_iter,main="Test MSE over 100 iterations", ylab="MSE")

#optional question - Ex-5
alpha <- 0.1
num_iters <- 100000 #defining large value of total iterations so that the code does not stop before the MSE change becomes less than 1%

#list to store test MSE in every iteration
testMSE_iter=vector()

#initialize coefficients in the range [-0.7,0.7]
beta <- matrix(runif(5, -0.7, 0.7),nrow=5)

#final iteration
finalIter=0

#gradient descent
for (j in 1:num_iters) {
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta <- beta - (alpha*delta)
  
  #get predictions on test set
  pred=vector()
  
  for (i in 1:length(test.Y)) {
    pred[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta)>=0.50) 1 else 0
    
  }
  testMSE_iter[j] <- sum((test.Y - pred)^2)/length(test.Y)
  if (j>10) {
  if (((testMSE_iter[j-10]-testMSE_iter[j])/testMSE_iter[j-10])<0.01) {
    finalIter <- j
    break
    
  } }
  
}

cat("Total iterations run before stopping: ",finalIter,"\n")
cat("Total iterations taken to get best MSE before simulation was stopped: ",finalIter-10)
cat("Final test MSE before stopping: ",testMSE_iter[j],"\n")

#ex-7 optional
alpha <- 0.1
num_iters <- 100

#initialize 4 beta vectors in the range [-0.7,0.7]
beta1 <- matrix(runif(5, -0.7, 0.7),nrow=5)
beta2 <- matrix(runif(5, -0.7, 0.7),nrow=5)
beta3 <- matrix(runif(5, -0.7, 0.7),nrow=5)
beta4 <- matrix(runif(5, -0.7, 0.7),nrow=5)

#gradient descent
for (i in 1:num_iters) {
  
  
  
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta1 <- beta1 - (alpha*delta)
  
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta2 <- beta2 - (alpha*delta)
  
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta3 <- beta3 - (alpha*delta)
  
  error <- (sigmoid(standardizedTrain.X%*%beta) - train.Y)
  delta <- (t(standardizedTrain.X) %*% error) / length(train.Y)
  beta4 <- beta4 - (alpha*delta)
}

#get predictions on test set
pred1=vector()
pred2=vector()
pred3=vector()
pred4=vector()
for (i in 1:length(test.Y)) {
  pred1[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta1)>=0.50) 1 else 0
  pred2[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta2)>=0.50) 1 else 0
  pred3[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta3)>=0.50) 1 else 0
  pred4[i] <- if(sigmoid(t(c(standardizedTest.X[i],standardizedTest.X[nrow(standardizedTest.X)+i],standardizedTest.X[nrow(standardizedTest.X)*2+i],standardizedTest.X[nrow(standardizedTest.X)*3+i],standardizedTest.X[nrow(standardizedTest.X)*4+i]))%*%beta4)>=0.50) 1 else 0
}

#get test MSE values of all 4 beta sets
testMSE1 <- sum((test.Y - pred1)^2)/length(test.Y)
testMSE2 <- sum((test.Y - pred2)^2)/length(test.Y)
testMSE3 <- sum((test.Y - pred3)^2)/length(test.Y)
testMSE4 <- sum((test.Y - pred4)^2)/length(test.Y)

#define a dictionary to store test MSE values in order with beta
testMSE_scores <- vector(mode="list", length=4)
names(testMSE_scores) <- c("Beta1", "Beta2", "Beta3", "Beta4")
testMSE_scores[[1]] <- testMSE1; testMSE_scores[[2]] <- testMSE2; testMSE_scores[[3]] <- testMSE3; testMSE_scores[[4]] <- testMSE4

leastMSE <- 0
bestBeta <- 0

#sorting MSE scores
for (i in 1:length(testMSE_scores))
{
  if (testMSE_scores$Beta1<testMSE_scores$Beta2 & testMSE_scores$Beta1<testMSE_scores$Beta3 & testMSE_scores$Beta1<testMSE_scores$Beta4)
  {
    leastMSE <- testMSE_scores$Beta1
    bestBeta <- beta1
  }
  else if(testMSE_scores$Beta2<testMSE_scores$Beta1 & testMSE_scores$Beta2<testMSE_scores$Beta3 & testMSE_scores$Beta2<testMSE_scores$Beta4)
  {
    leastMSE <- testMSE_scores$Beta2
    bestBeta <- beta2
  } 
  else if(testMSE_scores$Beta3<testMSE_scores$Beta1 & testMSE_scores$Beta3<testMSE_scores$Beta2 & testMSE_scores$Beta3<testMSE_scores$Beta4)
  {
    leastMSE <- testMSE_scores$Beta3
    bestBeta <- beta3
  }
  else
  {
    leastMSE <- testMSE_scores$Beta4
    bestBeta <- beta4
  }
  
}

cat("Best beta value was taken from beta values:\n",bestBeta,"\n with least MSE value as ",leastMSE)
