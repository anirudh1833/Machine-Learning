"Lending Club connects people who need money (borrowers) with people who have money (investors). Hopefully, as an investor you would want to invest in people who showed a profile of having a high probability of paying you back. We will try to create a model that will help predict this."
"Lending club had a very interesting year in 2016, so let's check out some of their data and keep the context in mind. This data is from before they even went public.

We will use lending data from 2007-2010 and be trying to classify and predict whether or not the borrower paid back their loan in full."

"Here are what the columns represent:

credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
purpose: The purpose of the loan (takes values 'credit_card', 'debt_consolidation', 'educational', 'major_purchase', 'small_business', and 'all_other').
int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
installment: The monthly installments owed by the borrower if the loan is funded.
log.annual.inc: The natural log of the self-reported annual income of the borrower.
dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
fico: The FICO credit score of the borrower.
days.with.cr.line: The number of days the borrower has had a credit line.
revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments)."

"Load the Data"
loans <- read.csv('C:\\Users\\anmuralidharan\\Documents\\Courses\\Github\\Machine Learning\\R\\loan_data.csv')

str(loans)

summary(loans)

"Conversion of columns to categorical data"
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

"Exploratory Data Analysis"

"Histogram of fico scores colored by not.fully.paid"
library(ggplot2)

pl <- ggplot(loans,aes(x=fico)) 
pl <- pl + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
pl + scale_fill_manual(values = c('green','red')) + theme_bw()

"Barplot of purpose counts, colored by not.fully.paid"
pl <- ggplot(loans,aes(x=factor(purpose))) 
pl <- pl + geom_bar(aes(fill=not.fully.paid),position = "dodge")
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

"Scatterplot of fico score versus int.rate."
ggplot(loans,aes(int.rate,fico)) +geom_point() + theme_bw()

ggplot(loans,aes(int.rate,fico)) +geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()

"Building the Model"

"Train and test sets"

library(caTools)

set.seed(101)

spl = sample.split(loans$not.fully.paid, 0.7)

train = subset(loans, spl == TRUE)

test = subset(loans, spl == FALSE)

library(e1071)

"Train a model on your training set"
model <- svm(not.fully.paid ~ .,data=train)

summary(model)

"Predict new values from the test set using your model"
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

"Tuning the Model for better results"
tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))
model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
