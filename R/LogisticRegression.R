"Predict if people in the data set belong in a certain class by salary, either making <=50k or >50k per year."

"Get the Data"
adult <- read.csv('C:\\Users\\anmuralidharan\\Documents\\Courses\\Github\\Machine Learning\\R\\adult_sal.csv')
head(adult)

"Drop the Repeated column"
library(dplyr)
adult <- select(adult,-X)

head(adult)
str(adult)
summary(adult)

"Data Cleaning - Reducing the number of factors"
"Frequency of the type_employer column"
table(adult$type_employer)

"Creating Unemployed group"
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)

"Combining Govt jobs and Self emp jobs"
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,group_emp)
table(adult$type_employer)

"Marital Column"
table(adult$marital)

"Reduce this to three groups:
  
1.Married
2.Not-Married
3.Never-Married"

group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

"Country Column"
table(adult$country)

"Grouping by Continents"
levels(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

table(adult$country)

str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

str(adult)

"Missing Data"
library(Amelia)
adult[adult == '?'] <- NA

table(adult$type_employer)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

"To Check the missing values"
missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

"Omit NA data from the adult data frame"
adult <- na.omit(adult)

"Check that all the NA values were in fact dropped."
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

"Exploratory Data Analysis"
library(ggplot2)
library(dplyr)

"Histogram of ages, colored by income"
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

"Histogram of hours worked per week"
ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()

names(adult)[names(adult)=="country"] <- "region"

"Barplot of region with the fill color defined by income class"
ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

"Building a Model"

"Import Library"
library(caTools)

"Set a random see so your random results are the same as this notebook"
set.seed(101) 

"Split up the sample, basically randomly assigns a booleans to a new column sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

"Training Data"
train = subset(adult, sample == TRUE)

"Testing Data"
test = subset(adult, sample == FALSE)


model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)

new.step.model <- step(model)
summary(new.step.model)

"Create a confusion matrix"
test$predicted.income = predict(model, newdata=test, type="response")

table(test$income, test$predicted.income > 0.5)

"Accuracy"
(6372+1423)/(6372+1423+548+872)

"Recall"
6732/(6372+548)


"Precision"
6732/(6372+872)
