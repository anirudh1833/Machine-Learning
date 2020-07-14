"Data - UCI archive based off of red and white wines"

df1 <- read.csv('C:\\Users\\anmuralidharan\\Documents\\Courses\\Github\\Machine Learning\\R\\winequality-red.csv',sep=';')
df2 <- read.csv('C:\\Users\\anmuralidharan\\Documents\\Courses\\Github\\Machine Learning\\R\\winequality-white.csv',sep=';')

"Adding a label column as 'red' or 'white' "
df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})

head(df1)
head(df2)

"Combine both dataframes into one"
wine <- rbind(df1,df2)

str(wine)

"Exploratory Data Analysis"
library(ggplot2)

"Histogram of residual sugar from the wine data"
pl <- ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)

# Optional adding of fill colors
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

"Histogram of citric.acid from the wine data"
pl <- ggplot(wine,aes(x=citric.acid)) + geom_histogram(aes(fill=label),color='black',bins=50)

# Optional adding of fill colors
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

"Histogram of alcohol from the wine data"
pl <- ggplot(wine,aes(x=alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50)

# Optional adding of fill colors
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

"Scatterplot of residual.sugar versus citric.acid, color by red and white wine"
pl <- ggplot(wine,aes(x=citric.acid,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)

# Optional adding of fill colors
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

"Scatterplot of volatile.acidity versus residual.sugar, color by red and white wine."
pl <- ggplot(wine,aes(x=volatile.acidity,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)

# Optional adding of fill colors
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

"Grab the wine data without the label"
clus.data <- wine[,1:12]

head(clus.data)

"Building the Clusters"

"Calling the KMeans function"
wine.cluster <- kmeans(wine[1:12],2)

print(wine.cluster$centers)

"Evaluating the Clusters"
table(wine$label,wine.cluster$cluster)
