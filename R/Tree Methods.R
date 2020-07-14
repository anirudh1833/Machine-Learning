"Tree methods to classify schools as Private or Public based off their features"

"Data - ISLR library, the College data frame."

"A data frame with 777 observations on the following 18 variables.

Private A factor with levels No and Yes indicating private or public university
Apps Number of applications received
Accept Number of applications accepted
Enroll Number of new students enrolled
Top10perc Pct. new students from top 10% of H.S. class
Top25perc Pct. new students from top 25% of H.S. class
F.Undergrad Number of fulltime undergraduates
P.Undergrad Number of parttime undergraduates
Outstate Out-of-state tuition
Room.Board Room and board costs
Books Estimated book costs
Personal Estimated personal spending
PhD Pct. of faculty with Ph.D.'s
Terminal Pct. of faculty with terminal degree
S.F.Ratio Student/faculty ratio
perc.alumni Pct. alumni who donate
Expend Instructional expenditure per student
Grad.Rate Graduation rate. "

"Load the Data"
library(ISLR)

head(College)

df<-College

"Exploratory Data Analysis"

"Scatterplot of Grad.Rate versus Room.Board, colored by the Private column"
library(ggplot2)
ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))

"Histogram of full time undergrad students, color by Private"
ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50)

"Histogram of Grad.Rate colored by Private."
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50)

"Train Test Split"
library(caTools)

set.seed(101) 

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

"Decision Tree"
library(rpart)
tree <- rpart(Private ~.,method='class',data = train)

"Predict the Private label on the test data"
tree.preds <- predict(tree,test)
head(tree.preds)


tree.preds <- as.data.frame(tree.preds)
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner)

head(tree.preds)

"Confusion matrix"

table(tree.preds$Private,test$Private)

"Plot the tree model"
library(rpart.plot)
prp(tree)

"Random Forest"
library(randomForest)

rf.model <- randomForest(Private ~ . , data = train,importance = TRUE)

"Confusion matrix on its own training set"
rf.model$confusion

rf.model$importance

"Predictions"

p <- predict(rf.model,test)
table(p,test$Private)
