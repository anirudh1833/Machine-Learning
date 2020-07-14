"Data - Iris Data Set"

"Use the ISLR libary to get the iris data set"
library(ISLR)

head(iris)

str(iris)

"Standardize the Data"

"Standardize the feature columns of the iris dataset"
stand.features <- scale(iris[1:4])

"Checking the variance of one of the new columns"
var(stand.features[,1])

"Join the standardized data with the response/target/label column"
final.data <- cbind(stand.features,iris[5])

head(final.data)

"Train and Test Splits"
set.seed(101)

library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

"Build a KNN Model"
library(class)

"Predict species of the test set"
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
predicted.species

"Misclassification Rate"
mean(test$Species != predicted.species)

"Choosing a K value"

"Create a plot of the error (misclassification) rate for k values ranging from 1 to 10."

predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red')

"Error drops to its lowest for k values between 2-6"
