######################################################
#BNP Kaggle competition
#Model 1
#Predicting whether a claim is suitable for accelerated
#approval (0 or 1 outcome variable)
#
#######################################################

#rename function
library(plyr)
library(gbm)


#import datasets
train <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\train.csv')
test <- read.csv('C:\\Users\\Randy\\Downloads\\Kaggle BNP\\test.csv')


#sets the working directory
setwd('C:/Users/Randy/Downloads/Kaggle BNP/ML3')

#Includes functions in the BNP Function script
source("BNP functions.R")


#calls the split function to divide the train dataset
bothFrames = split(train)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]











outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=2))
outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb"))

outputFrame = cbind(outputFrame, test2$target)
outputFrame[,1] = test2[,1]
outputFrame[,2] = .76

log_loss(outputFrame)



#runs gbm model
output = gbmParse(train2,test2)



test2 = Normalize(test2)
#backups
train5 = train2
test5 = test2



naDetect <- function (train,test)
{
	largest= 0
	most = 1
	for (i in 1:ncol(train))
	{
		if(sum(is.na(train[,i])) > largest)
		{
			largest = sum(is.na(train[,i]));
			most = i;
		}
	}

}



















