#Loading Libraries
# Call libraries and read data
library(caret)
library(party)
library(rpart)
library(rpart.plot)
library(e1071)
library(corrplot)
library(dplyr)          # For data manipulation ,filtering etc
library(magrittr)       # For pipes
library(caret)          # For dummy variables, nearZeroVar(), findCorrleation()
library(ggplot2)        # For plotting
library(ggthemes)       # For a variety of plot-themes
library(gridExtra)      # Arranging ggplots in a grid
library(lattice)
library(vegan)
library(NbClust)
library(cluster)        # For silhoutte()
library(factoextra)     # get_clust_tendency() assesses hopkins stat
library(clustertend)    # Another package for hopkins() function
library(data.table)
library(GGally)
library(ggcorrplot)
library(mclust)
library(fpc)

wine_data = read.csv("winequality-red.csv",sep=";") # read in the data in a dataframe


#Some more details related to data.
#How data looks
print("---Data preview---")
head(wine_data)

#Number of rows to identify how big is the data we are dealing with
print("---Number of rows---")
nrow(wine_data)

print("---Column names---")
#Names of all columns
names(wine_data)


#Let us check if we have any NA values in our data.
#We should remove and NA or incomplete data.
#If FALSE means no NA data in our data.frame
#If TRUE we will check each column and for NA data.

print("----Checking for NA Data---")
any(is.na.data.frame(wine_data))

for(i in 1:11){
  print(paste("---Plot for---", colnames(wine_data)[i]))
  
  #Overall distribution
  print(ggplot(wine_data, aes_string("quality", colnames(wine_data)[i]))+
          geom_count(col="tomato3", show.legend=F)+
          theme_solarized())

}

#Exploratory data analysis

#1)Here we see that fixed acidity does not give any specification to classify the quality.
#2)we see that its quite a downing trend in the volatile acidity as we go higher the quality.
#3)Composition of citric acid go higher as we go higher in the quality of the wine.
#4)we see that residual sugar does not give any specification to classify the quality
#5)Composition of chloride  go down as we go higher in the quality of the wine
#6)we see that free sulpher dioxide does not give any specification to classify the quality
#7)we see that total sulpher dioxide does not give any specification to classify the quality
#8)we see that density is quite a decreasing as we go higher in quality.
#9)we see that pH is also decreasing as we go higher in quality.
#10)Composition of sulphates go higher as we go higher in the quality of the wine.
#11)Composition of alcohol also go higher as we go higher in the quality of the wine.





#plotting count vs quality
ggplot(wine_data,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(min(wine_data['quality']),max(wine_data['quality']),1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()

#Plot shows that majority of the dataset has quality 5 and 6. so we will divide the the dataset into three bins of A,B and C
wine_data$quality = lapply(wine_data[,12], function (x)
{
  if(x > 5)  { "good"}
  else { "bad"}   
})
head(wine_data)











#unlist simplifies to produce a vector which contains all the atomic components of a list 
wine_data$quality = unlist(wine_data$quality)
wine_data$quality = as.factor(wine_data$quality)

#We will divide the data into train and test set with 80% for train data and 20% for test data.
sam <- sample(2, nrow(wine_data), replace=TRUE, prob=c(0.8, 0.2))
print((sam))
trainData <- wine_data[sam==1,]
testData <- wine_data[sam==2,]

table(trainData$quality)
table(testData$quality)


colnames(wine_data)

myFormula <- quality~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density +
  pH + sulphates + alcohol 

#Decision Tree
rpTree <- rpart(myFormula, method = "class", data=trainData)
rpart.plot(rpTree, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)

trainPred = predict(rpTree,trainData,type = "class")
confusionMatrix(trainData$quality,trainPred)

testPred = predict(rpTree,testData,type = "class")
confusionMatrix(testData$quality,testPred)


