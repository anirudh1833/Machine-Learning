"DATA
The data has the following features:

datetime - hourly date + timestamp
season - 1 = spring, 2 = summer, 3 = fall, 4 = winter
holiday - whether the day is considered a holiday
workingday - whether the day is neither a weekend nor holiday
weather -
1: Clear, Few clouds, Partly cloudy, Partly cloudy
2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
temp - temperature in Celsius
atemp - feels like temperature in Celsius
humidity - relative humidity
windspeed - wind speed
casual - number of non-registered user rentals initiated
registered - number of registered user rentals initiated
count - number of total rentals"

bike <- read.csv('C:\\Users\\anmuralidharan\\Documents\\Courses\\Github\\Machine Learning\\R\\bikeshare.csv')

head(bike)

"Exploratory Data Analysis"
"Scatter plot of count vs temp"
library(ggplot2)
ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()

"Converting the datetime column into POSIXct before plotting."
bike$datetime <- as.POSIXct(bike$datetime)
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

"Correlation between temp and count"
cor(bike[,c('temp','count')])

"Boxplot for different seasons"
ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()

"Feature Engineering"
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})
head(bike)

"Scatterplot of count versus hour, with color scale based on temp. Only use bike data where workingday==1."
library(dplyr)
pl <- ggplot(filter(bike,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

"Plot for non working days"
pl <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

"Building the Model"
temp.model <- lm(count~temp,bike)
summary(temp.model)

"Prediction"
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

bike$hour <- sapply(bike$hour,as.numeric)

"Build a model that attempts to predict count based off of the following features"
model <- lm(count ~ . -casual - registered -datetime -atemp,bike )
summary(model)
