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
library(h2o)

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



#runs deep learning model
myout = deepL(train2[,-c(3:85)] , test2[,-c(3:85)],explan) 

#runs gbm model
output = gbmParse(train2,test2)



##########################################################################################
#ensemble of gbm with 300 trees and all variables and h2o.deeplearning with variables 3:70
#.5 h20.deeplearning(.4756146) .5 gbm (.4689628) = .4625459
#
#
#########################################################################################



ensembleFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
ensembleFrame = rename(ensembleFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))


ensembleFrame[,1] = myout[,1]
ensembleFrame[,3] = myout[,3]
ensembleFrame[,2] = .03 * myout[,2] + .97* output[,2]

log_loss(ensembleFrame)


#tests the strength of the prediction versus the actual outcome
cor(cbind(output[,2],myout[,2], ensembleFrame[,2],outputFrame[,3]))










outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=2))
outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb"))

outputFrame = cbind(outputFrame, test2$target)
outputFrame[,1] = test2[,1]
outputFrame[,2] = .76

log_loss(outputFrame)


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



















